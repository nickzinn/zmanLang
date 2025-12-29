/*
StackVM-32 Interpreter (v1)
---------------------------
Runs a single .zvm container produced by the assembler. Implements the 50-op ISA spec.

Build:
  cc -O2 -std=c11 -Wall -Wextra -o svm_vm svm_vm.c

Usage:
  ./svm_vm program.zvm [stack_words]

Defaults:
  stack_words = 65536 (256 KiB stack)

Syscalls (SYSCALL u8):
  0: exit(code)             stack: code -> (halts)
  1: print_u32(x)           stack: x ->
  2: print_i32(x)           stack: x ->
  3: putchar(ch)            stack: ch ->
  4: write(ptr,len)         stack: ptr len ->   (writes bytes from linear memory to stdout)
  5: read(ptr,len)          stack: ptr len -> n (reads into linear memory from stdin, returns n)

TRAP u16:
  Halts with trap code (u16).

Notes:
  - Little-endian memory LOAD32/STORE32
  - MEMCPY behaves like memmove (overlap-safe)
  - Jumps/calls use absolute addr32 into code blob
  - FP convention: CALL pushes ret_ip, old_fp; then fp=sp (after pushes)
  - LDFP/STFP use fp + s16 word slots (index into value stack)

*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>

typedef struct {
  // Optional pointer to the ZVM container backing vm->code.
  // If owns_container=1, VM cleanup will free it.
  uint8_t* container;
  size_t container_len;
  int owns_container;

  uint8_t* code;
  uint32_t code_size;

  // Instruction-start bitset produced by the verifier. Used to validate dynamic
  // control-flow targets (RET/CALLI) without per-instruction bounds checks.
  uint8_t* code_starts;
  uint32_t code_starts_len; // in bytes

  uint8_t* mem;
  uint32_t mem_size;

  uint32_t* stack;
  uint32_t stack_cap; // in words

  uint32_t ip; // byte index into code
  uint32_t sp; // next free word index
  uint32_t fp; // frame pointer index into stack (see spec)

  int halted;
  uint32_t trap_code;
  const char* trap_msg;

  // Output capture (useful for browser/WASM builds).
  uint8_t* out;
  size_t out_len;
  size_t out_cap;

  // Host I/O behavior
  int stream_stdio;   // if true, use stdio for syscalls 1-5
  int capture_output; // if true, capture writes/prints into out
} VM;

// ------------------------------ Helpers ------------------------------

static void die(const char* msg) {
  fprintf(stderr, "error: %s\n", msg);
  exit(1);
}

static void* xmalloc(size_t n) {
  void* p = malloc(n ? n : 1);
  if (!p) die("out of memory");
  return p;
}

static void* xrealloc(void* p, size_t n) {
  void* q = realloc(p, n ? n : 1);
  if (!q) die("out of memory");
  return q;
}

static uint8_t* read_file(const char* path, size_t* out_len) {
  FILE* f = fopen(path, "rb");
  if (!f) {
    fprintf(stderr, "error: cannot open '%s': %s\n", path, strerror(errno));
    exit(1);
  }
  fseek(f, 0, SEEK_END);
  long n = ftell(f);
  if (n < 0) die("ftell failed");
  fseek(f, 0, SEEK_SET);
  uint8_t* buf = (uint8_t*)xmalloc((size_t)n);
  size_t rd = fread(buf, 1, (size_t)n, f);
  fclose(f);
  if (rd != (size_t)n) die("file read failed");
  *out_len = (size_t)n;
  return buf;
}

static void out_reserve(VM* vm, size_t add) {
  size_t need = vm->out_len + add;
  if (need <= vm->out_cap) return;
  size_t ncap = vm->out_cap ? vm->out_cap : 256;
  while (ncap < need) ncap *= 2;
  vm->out = (uint8_t*)xrealloc(vm->out, ncap);
  vm->out_cap = ncap;
}

static void out_write(VM* vm, const void* data, size_t len) {
  if (!vm->capture_output || len == 0) return;
  out_reserve(vm, len);
  memcpy(vm->out + vm->out_len, data, len);
  vm->out_len += len;
}

static void out_write_u8(VM* vm, uint8_t b) {
  out_write(vm, &b, 1);
}

static void out_clear(VM* vm) {
  vm->out_len = 0;
}

// ------------------------------ ZVM Container ------------------------------
// Header (28 bytes, little-endian):
//  0  char[4] magic = "ZVM1"
//  4  u16 version   (1)
//  6  u16 flags     (0)
//  8  u32 code_size
// 12  u32 mem_init_size
// 16  u32 mem_total_size
// 20  u32 entry_ip
// 24  u32 reserved (0)
// 28  code bytes
// ..  mem init bytes
static uint32_t read_u32_le(const uint8_t* p);

typedef struct {
  uint32_t code_size;
  uint32_t mem_init_size;
  uint32_t mem_total_size;
  uint32_t entry_ip;
} ZvmHeader;

static int read_zvm_header(const uint8_t* buf, size_t len, ZvmHeader* out) {
  if (len < 28) return 0;
  if (!(buf[0]=='Z' && buf[1]=='V' && buf[2]=='M' && buf[3]=='1')) return 0;
  uint16_t version = (uint16_t)(buf[4] | ((uint16_t)buf[5] << 8));
  if (version != 1) return 0;
  // flags currently ignored
  out->code_size     = read_u32_le(buf + 8);
  out->mem_init_size = read_u32_le(buf + 12);
  out->mem_total_size= read_u32_le(buf + 16);
  out->entry_ip      = read_u32_le(buf + 20);
  return 1;
}

static uint32_t read_u32_le(const uint8_t* p) {
  return (uint32_t)p[0]
      | ((uint32_t)p[1] << 8)
      | ((uint32_t)p[2] << 16)
      | ((uint32_t)p[3] << 24);
}

static uint16_t read_u16_le(const uint8_t* p) {
  return (uint16_t)(p[0] | ((uint16_t)p[1] << 8));
}

static int16_t read_s16_le(const uint8_t* p) {
  return (int16_t)read_u16_le(p);
}

// One-time verifier: ensures instructions are not truncated and that static
// branch/call targets land on instruction boundaries.
static int vm_verify_code(const uint8_t* code,
                          uint32_t code_size,
                          uint32_t entry_ip,
                          uint8_t** out_starts,
                          uint32_t* out_starts_len,
                          const char** out_err) {
  if (out_starts) *out_starts = NULL;
  if (out_starts_len) *out_starts_len = 0;
  if (out_err) *out_err = NULL;
  if (!code) {
    if (out_err) *out_err = "null code";
    return 0;
  }
  if (code_size == 0) {
    if (out_err) *out_err = "empty code";
    return 0;
  }
  if (entry_ip >= code_size) {
    if (out_err) *out_err = "entry_ip out of range";
    return 0;
  }

  uint32_t bits_len = (code_size + 7u) / 8u;
  uint8_t* starts = (uint8_t*)calloc(bits_len ? (size_t)bits_len : 1u, 1);
  if (!starts) {
    if (out_err) *out_err = "out of memory (verify starts)";
    return 0;
  }

  // Worst-case allocation: very small code, or degenerate input.
  uint32_t* targets = (uint32_t*)xmalloc((size_t)code_size * sizeof(uint32_t));
  uint32_t tcount = 0;

#define SET_START(_ip) do { starts[(_ip) >> 3] = (uint8_t)(starts[(_ip) >> 3] | (uint8_t)(1u << ((_ip) & 7u))); } while (0)
#define IS_START(_ip)  ((starts[(_ip) >> 3] >> ((_ip) & 7u)) & 1u)

  const char* err = NULL;
  uint32_t ip = 0;
  while (ip < code_size) {
    SET_START(ip);
    uint8_t op = code[ip++];
    uint32_t imm = 0;

    switch (op) {
      case 0x00: /* NOP */ break;
      case 0x01: /* HALT */ break;
      case 0x02: /* SYSCALL u8 */ imm = 1; break;
      case 0x03: /* TRAP u16 */ imm = 2; break;
      case 0x04: /* JMP addr32 */ imm = 4; break;
      case 0x05: /* JZ addr32 */ imm = 4; break;
      case 0x06: /* JNZ addr32 */ imm = 4; break;
      case 0x07: /* PUSHI u32 */ imm = 4; break;
      case 0x08: /* POP */ break;
      case 0x09: /* DUP */ break;
      case 0x0A: /* DUP2 */ break;
      case 0x0B: /* SWAP */ break;
      case 0x0C: /* ROT */ break;
      case 0x0D: /* OVER */ break;
      case 0x0E: /* CALL addr32 */ imm = 4; break;
      case 0x0F: /* RET u8 */ imm = 1; break;
      case 0x10: /* ENTER u16 */ imm = 2; break;
      case 0x11: /* LEAVE */ break;
      case 0x12: /* LDFP s16 */ imm = 2; break;
      case 0x13: /* STFP s16 */ imm = 2; break;
      case 0x14: /* LOAD32 */ break;
      case 0x15: /* STORE32 */ break;
      case 0x16: /* LOAD8U */ break;
      case 0x17: /* STORE8 */ break;
      case 0x18: /* MEMCPY */ break;
      case 0x19: /* ADD */ break;
      case 0x1A: /* SUB */ break;
      case 0x1B: /* MUL */ break;
      case 0x1C: /* DIVS */ break;
      case 0x1D: /* NEG */ break;
      case 0x1E: /* AND */ break;
      case 0x1F: /* OR */ break;
      case 0x20: /* XOR */ break;
      case 0x21: /* SHL */ break;
      case 0x22: /* SHR */ break;
      case 0x23: /* EQ */ break;
      case 0x24: /* LT */ break;
      case 0x25: /* GT */ break;
      case 0x26: /* LE */ break;
      case 0x27: /* GE */ break;
      case 0x28: /* ADDI s16 */ imm = 2; break;
      case 0x29: /* SUBI s16 */ imm = 2; break;
      case 0x2A: /* INC */ break;
      case 0x2B: /* DEC */ break;
      case 0x2C: /* MODS */ break;
      case 0x2D: /* NOT */ break;
      case 0x2E: /* CALLI */ break;
      case 0x2F: /* TAILCALL addr32 */ imm = 4; break;
      case 0x30: /* LOAD_OFF s16 */ imm = 2; break;
      case 0x31: /* STORE_OFF s16 */ imm = 2; break;
      default:
        err = "unknown opcode";
        goto fail;
    }

    if (ip + imm > code_size) {
      err = "truncated instruction";
      goto fail;
    }

    // Record static branch/call targets for validation against instruction boundaries.
    if (imm == 4u) {
      if (op == 0x04 || op == 0x05 || op == 0x06 || op == 0x0E || op == 0x2F) {
        uint32_t target = read_u32_le(code + ip);
        targets[tcount++] = target;
      }
    }

    ip += imm;
  }

  if (!IS_START(entry_ip)) {
    err = "entry_ip not on instruction boundary";
    goto fail;
  }

  for (uint32_t i = 0; i < tcount; i++) {
    uint32_t t = targets[i];
    if (t >= code_size) {
      err = "branch target out of range";
      goto fail;
    }
    if (!IS_START(t)) {
      err = "branch target not on instruction boundary";
      goto fail;
    }
  }

  free(targets);
  if (out_starts) *out_starts = starts;
  if (out_starts_len) *out_starts_len = bits_len;
  return 1;

fail:
  if (out_err) *out_err = err ? err : "verification failed";
  free(targets);
  free(starts);
  return 0;

#undef IS_START
#undef SET_START
}

static inline int code_is_start(const VM* vm, uint32_t ip) {
  if (!vm->code_starts) return 0;
  if (ip >= vm->code_size) return 0;
  uint32_t byte = ip >> 3;
  uint32_t bit = ip & 7u;
  if (byte >= vm->code_starts_len) return 0;
  return ((vm->code_starts[byte] >> bit) & 1u) != 0u;
}

static void write_u32_mem_le(uint8_t* mem, uint32_t addr, uint32_t v) {
  mem[addr + 0] = (uint8_t)(v & 0xFF);
  mem[addr + 1] = (uint8_t)((v >> 8) & 0xFF);
  mem[addr + 2] = (uint8_t)((v >> 16) & 0xFF);
  mem[addr + 3] = (uint8_t)((v >> 24) & 0xFF);
}

static uint32_t read_u32_mem_le(const uint8_t* mem, uint32_t addr) {
  return (uint32_t)mem[addr + 0]
      | ((uint32_t)mem[addr + 1] << 8)
      | ((uint32_t)mem[addr + 2] << 16)
      | ((uint32_t)mem[addr + 3] << 24);
}

static void trap(VM* vm, uint32_t code, const char* msg) {
  vm->halted = 1;
  vm->trap_code = code;
  vm->trap_msg = msg;
}

static int check_stack_push(VM* vm, uint32_t n) {
  if (vm->sp + n > vm->stack_cap) {
    trap(vm, 0xFFFF0002u, "stack overflow");
    return 0;
  }
  return 1;
}

static int check_stack_pop(VM* vm, uint32_t n) {
  if (vm->sp < n) {
    trap(vm, 0xFFFF0003u, "stack underflow");
    return 0;
  }
  return 1;
}

static void push(VM* vm, uint32_t v) {
  vm->stack[vm->sp++] = v;
}

static uint32_t pop(VM* vm) {
  return vm->stack[--vm->sp];
}

static int mem_range_ok(VM* vm, uint32_t addr, uint32_t len) {
  // allow len=0
  if (len == 0) return 1;
  if (addr > vm->mem_size) return 0;
  if (len > vm->mem_size) return 0;
  if (addr + len < addr) return 0; // overflow
  return addr + len <= vm->mem_size;
}

static uint32_t mask_shift(uint32_t s) { return s & 31u; }

// ------------------------------ Syscalls ------------------------------

static void do_syscall(VM* vm, uint8_t id) {
  switch (id) {
    case 0: { // exit(code)
      if (!check_stack_pop(vm, 1)) return;
      uint32_t code = pop(vm);
      vm->halted = 1;
      vm->trap_code = code;
      vm->trap_msg = "exit";
      return;
    }
    case 1: { // print_u32(x)
      if (!check_stack_pop(vm, 1)) return;
      uint32_t x = pop(vm);
      char buf[32];
      int n = snprintf(buf, sizeof(buf), "%u", x);
      if (n > 0) {
        if (vm->stream_stdio) { fwrite(buf, 1, (size_t)n, stdout); fflush(stdout); }
        out_write(vm, buf, (size_t)n);
      }
      return;
    }
    case 2: { // print_i32(x)
      if (!check_stack_pop(vm, 1)) return;
      int32_t x = (int32_t)pop(vm);
      char buf[32];
      int n = snprintf(buf, sizeof(buf), "%d", x);
      if (n > 0) {
        if (vm->stream_stdio) { fwrite(buf, 1, (size_t)n, stdout); fflush(stdout); }
        out_write(vm, buf, (size_t)n);
      }
      return;
    }
    case 3: { // putchar(ch)
      if (!check_stack_pop(vm, 1)) return;
      uint32_t ch = pop(vm);
      uint8_t b = (uint8_t)(ch & 0xFFu);
      if (vm->stream_stdio) { fputc((int)b, stdout); fflush(stdout); }
      out_write_u8(vm, b);
      return;
    }
    case 4: { // write(ptr,len)
      if (!check_stack_pop(vm, 2)) return;
      uint32_t len = pop(vm);
      uint32_t ptr = pop(vm);
      if (!mem_range_ok(vm, ptr, len)) { trap(vm, 0xFFFF0100u, "syscall write OOB"); return; }
      if (len) {
        if (vm->stream_stdio) { fwrite(vm->mem + ptr, 1, len, stdout); fflush(stdout); }
        out_write(vm, vm->mem + ptr, (size_t)len);
      }
      return;
    }
    case 5: { // read(ptr,len) -> n
      if (!check_stack_pop(vm, 2)) return;
      uint32_t len = pop(vm);
      uint32_t ptr = pop(vm);
      if (!mem_range_ok(vm, ptr, len)) { trap(vm, 0xFFFF0101u, "syscall read OOB"); return; }
      size_t n = 0;
      if (vm->stream_stdio) {
        if (len) n = fread(vm->mem + ptr, 1, len, stdin);
      } else {
        // Browser/WASM builds can provide input via a custom host layer.
        // Default: return 0 bytes read.
        n = 0;
      }
      if (!check_stack_push(vm, 1)) return;
      push(vm, (uint32_t)n);
      return;
    }
    default:
      trap(vm, 0xFFFF00FFu, "unknown syscall id");
      return;
  }
}

// ------------------------------ Dispatch ------------------------------

static inline __attribute__((always_inline)) void step(VM* vm) {
  // Cache hot fields in locals (helps WASM and native).
  uint8_t* code = vm->code;
  uint32_t code_size = vm->code_size;
  uint8_t* mem = vm->mem;
  uint32_t mem_size = vm->mem_size;
  uint32_t* stack = vm->stack;
  uint32_t stack_cap = vm->stack_cap;
  uint32_t ip = vm->ip;
  uint32_t sp = vm->sp;
  uint32_t fp = vm->fp;

#define TRAP(_code, _msg) do { trap(vm, (_code), (_msg)); goto end; } while (0)
#define CHECK_PUSH(_n) do { if (sp + (uint32_t)(_n) > stack_cap) TRAP(0xFFFF0002u, "stack overflow"); } while (0)
#define CHECK_POP(_n) do { if (sp < (uint32_t)(_n)) TRAP(0xFFFF0003u, "stack underflow"); } while (0)
#define PUSH(_v) do { stack[sp++] = (_v); } while (0)
#define POP() (stack[--sp])

#define MEM_RANGE_OK(_addr, _len) \
  ((_len) == 0u || \
   ((_addr) <= mem_size && \
    (_len) <= mem_size && \
    ((_addr) + (_len)) >= (_addr) && \
    ((_addr) + (_len)) <= mem_size))

#define MEM_ADDR_OK(_addr) ((_addr) < mem_size)
#define MEM_ADDR32_OK(_addr) (((_addr) + 4u) <= mem_size)

  // Bytecode is verified at load time, but dynamic control-flow (RET/CALLI)
  // can still attempt invalid targets. Keep a single guard for safety.
  if (ip >= code_size) TRAP(0xFFFF0001u, "IP out of range");
  uint8_t op = code[ip++];

  switch (op) {
    // 0x00 NOP
    case 0x00:
      break;

    // 0x01 HALT
    case 0x01:
      vm->halted = 1;
      vm->trap_code = 0;
      vm->trap_msg = "halt";
      goto end;

    // 0x02 SYSCALL u8
    case 0x02: {
      uint8_t id = code[ip++];
      // Sync locals before calling helper.
      vm->ip = ip;
      vm->sp = sp;
      vm->fp = fp;
      do_syscall(vm, id);
      // Reload locals after syscall.
      ip = vm->ip;
      sp = vm->sp;
      fp = vm->fp;
      // Fast-path reload of pointers in case VM was reinitialized (shouldn't happen here).
      code = vm->code;
      code_size = vm->code_size;
      mem = vm->mem;
      mem_size = vm->mem_size;
      stack = vm->stack;
      stack_cap = vm->stack_cap;
      break;
    }

    // 0x03 TRAP u16
    case 0x03: {
      uint16_t tc = read_u16_le(code + ip);
      ip += 2;
      trap(vm, (uint32_t)tc, "TRAP");
      goto end;
    }

    // 0x04 JMP addr32
    case 0x04: {
      uint32_t a = read_u32_le(code + ip);
      ip = a;
      break;
    }

    // 0x05 JZ addr32
    case 0x05: {
      uint32_t a = read_u32_le(code + ip);
      ip += 4;
      CHECK_POP(1);
      uint32_t c = POP();
      if (c == 0) {
        ip = a;
      }
      break;
    }

    // 0x06 JNZ addr32
    case 0x06: {
      uint32_t a = read_u32_le(code + ip);
      ip += 4;
      CHECK_POP(1);
      uint32_t c = POP();
      if (c != 0) {
        ip = a;
      }
      break;
    }

    // 0x07 PUSHI u32
    case 0x07: {
      uint32_t imm = read_u32_le(code + ip);
      ip += 4;
      CHECK_PUSH(1);
      PUSH(imm);
      break;
    }

    // 0x08 POP
    case 0x08:
      CHECK_POP(1);
      sp--;
      break;

    // 0x09 DUP
    case 0x09:
      CHECK_POP(1);
      CHECK_PUSH(1);
      {
        uint32_t t = stack[sp - 1];
        PUSH(t);
      }
      break;

    // 0x0A DUP2
    case 0x0A:
      CHECK_POP(2);
      CHECK_PUSH(2);
      {
        uint32_t a = stack[sp - 2];
        uint32_t b = stack[sp - 1];
        PUSH(a);
        PUSH(b);
      }
      break;

    // 0x0B SWAP
    case 0x0B:
      CHECK_POP(2);
      {
        uint32_t b = POP();
        uint32_t a = POP();
        PUSH(b);
        PUSH(a);
      }
      break;

    // 0x0C ROT: a b c -> b c a
    case 0x0C:
      CHECK_POP(3);
      {
        uint32_t c = POP();
        uint32_t b = POP();
        uint32_t a = POP();
        PUSH(b);
        PUSH(c);
        PUSH(a);
      }
      break;

    // 0x0D OVER: a b -> a b a
    case 0x0D:
      CHECK_POP(2);
      CHECK_PUSH(1);
      {
        uint32_t t = stack[sp - 2];
        PUSH(t);
      }
      break;

    // 0x0E CALL addr32
    case 0x0E: {
      uint32_t target = read_u32_le(code + ip);
      ip += 4;
      CHECK_PUSH(2);
      uint32_t ret_ip = ip;
      PUSH(ret_ip);
      PUSH(fp);
      fp = sp;
      ip = target;
      break;
    }

    // 0x2E CALLI (addr on stack)
    case 0x2E: {
      CHECK_POP(1);
      uint32_t target = POP();
      if (target >= code_size || !code_is_start(vm, target)) TRAP(0xFFFF002Eu, "CALLI to invalid address");
      CHECK_PUSH(2);
      uint32_t ret_ip = ip;
      PUSH(ret_ip);
      PUSH(fp);
      fp = sp;
      ip = target;
      break;
    }

    // 0x10 ENTER u16
    case 0x10: {
      uint16_t nlocals = read_u16_le(code + ip);
      ip += 2;
      CHECK_PUSH(nlocals);
      for (uint16_t i = 0; i < nlocals; i++) PUSH(0);
      break;
    }

    // 0x11 LEAVE
    case 0x11:
      if (fp > sp) TRAP(0xFFFF0011u, "LEAVE with corrupted fp");
      sp = fp;
      break;

    // 0x12 LDFP s16
    case 0x12: {
      int16_t off = read_s16_le(code + ip);
      ip += 2;
      int64_t idx = (int64_t)fp + (int64_t)off;
      if (idx < 0 || idx >= (int64_t)sp) TRAP(0xFFFF0012u, "LDFP index out of range");
      CHECK_PUSH(1);
      PUSH(stack[(uint32_t)idx]);
      break;
    }

    // 0x13 STFP s16
    case 0x13: {
      int16_t off = read_s16_le(code + ip);
      ip += 2;
      CHECK_POP(1);
      uint32_t val = POP();
      int64_t idx = (int64_t)fp + (int64_t)off;
      if (idx < 0 || idx >= (int64_t)sp) TRAP(0xFFFF0013u, "STFP index out of range");
      stack[(uint32_t)idx] = val;
      break;
    }

    // 0x0F RET u8
    case 0x0F: {
      uint8_t argc = code[ip++];
      CHECK_POP(1);
      uint32_t rv = POP();

      if (fp > sp) TRAP(0xFFFF000Fu, "RET with corrupted fp");
      sp = fp;

      CHECK_POP(2);
      uint32_t old_fp = POP();
      uint32_t ret_ip = POP();

      if (sp < (uint32_t)argc) TRAP(0xFFFF000Fu, "RET argc underflow");
      sp -= (uint32_t)argc;

      CHECK_PUSH(1);
      PUSH(rv);

      fp = old_fp;
      if (ret_ip >= code_size || !code_is_start(vm, ret_ip)) TRAP(0xFFFF000Fu, "RET to invalid ip");
      ip = ret_ip;
      break;
    }

    // 0x2F TAILCALL addr32
    case 0x2F: {
      uint32_t target = read_u32_le(code + ip);
      ip = target;
      break;
    }

    // 0x14 LOAD32
    case 0x14: {
      CHECK_POP(1);
      uint32_t addr = POP();
      if (!MEM_ADDR32_OK(addr)) TRAP(0xFFFF0014u, "LOAD32 OOB");
      uint32_t v = read_u32_mem_le(mem, addr);
      CHECK_PUSH(1);
      PUSH(v);
      break;
    }

    // 0x15 STORE32
    case 0x15: {
      CHECK_POP(2);
      uint32_t val = POP();
      uint32_t addr = POP();
      if (!MEM_ADDR32_OK(addr)) TRAP(0xFFFF0015u, "STORE32 OOB");
      write_u32_mem_le(mem, addr, val);
      break;
    }

    // 0x16 LOAD8U
    case 0x16: {
      CHECK_POP(1);
      uint32_t addr = POP();
      if (!MEM_ADDR_OK(addr)) TRAP(0xFFFF0016u, "LOAD8U OOB");
      CHECK_PUSH(1);
      PUSH((uint32_t)mem[addr]);
      break;
    }

    // 0x17 STORE8
    case 0x17: {
      CHECK_POP(2);
      uint32_t val = POP();
      uint32_t addr = POP();
      if (!MEM_ADDR_OK(addr)) TRAP(0xFFFF0017u, "STORE8 OOB");
      mem[addr] = (uint8_t)(val & 0xFFu);
      break;
    }

    // 0x18 MEMCPY (memmove semantics): dest src len ->
    case 0x18: {
      CHECK_POP(3);
      uint32_t len  = POP();
      uint32_t src  = POP();
      uint32_t dest = POP();
      if (!MEM_RANGE_OK(src, len) || !MEM_RANGE_OK(dest, len)) TRAP(0xFFFF0018u, "MEMCPY OOB");
      memmove(mem + dest, mem + src, (size_t)len);
      break;
    }

    // 0x19 ADD
    case 0x19: {
      CHECK_POP(2);
      uint32_t b = POP(), a = POP();
      PUSH(a + b);
      break;
    }

    // 0x1A SUB
    case 0x1A: {
      CHECK_POP(2);
      uint32_t b = POP(), a = POP();
      PUSH(a - b);
      break;
    }

    // 0x1B MUL
    case 0x1B: {
      CHECK_POP(2);
      uint32_t b = POP(), a = POP();
      PUSH(a * b);
      break;
    }

    // 0x1C DIVS (signed)
    case 0x1C: {
      CHECK_POP(2);
      int32_t b = (int32_t)POP();
      int32_t a = (int32_t)POP();
      if (b == 0) TRAP(0xFFFF001Cu, "DIVS divide by zero");
      if (a == (int32_t)0x80000000 && b == -1) TRAP(0xFFFF001Cu, "DIVS overflow");
      PUSH((uint32_t)(a / b));
      break;
    }

    // 0x1D NEG
    case 0x1D: {
      CHECK_POP(1);
      uint32_t a = POP();
      PUSH((uint32_t)(-(int32_t)a));
      break;
    }

    // 0x1E AND
    case 0x1E: {
      CHECK_POP(2);
      uint32_t b = POP(), a = POP();
      PUSH(a & b);
      break;
    }

    // 0x1F OR
    case 0x1F: {
      CHECK_POP(2);
      uint32_t b = POP(), a = POP();
      PUSH(a | b);
      break;
    }

    // 0x20 XOR
    case 0x20: {
      CHECK_POP(2);
      uint32_t b = POP(), a = POP();
      PUSH(a ^ b);
      break;
    }

    // 0x21 SHL
    case 0x21: {
      CHECK_POP(2);
      uint32_t sh = POP(), v = POP();
      PUSH(v << mask_shift(sh));
      break;
    }

    // 0x22 SHR (logical)
    case 0x22: {
      CHECK_POP(2);
      uint32_t sh = POP(), v = POP();
      PUSH(v >> mask_shift(sh));
      break;
    }

    // 0x23 EQ
    case 0x23: {
      CHECK_POP(2);
      uint32_t b = POP(), a = POP();
      PUSH((a == b) ? 1u : 0u);
      break;
    }

    // 0x24 LT (signed)
    case 0x24: {
      CHECK_POP(2);
      int32_t b = (int32_t)POP(), a = (int32_t)POP();
      PUSH((a < b) ? 1u : 0u);
      break;
    }

    // 0x25 GT (signed)
    case 0x25: {
      CHECK_POP(2);
      int32_t b = (int32_t)POP(), a = (int32_t)POP();
      PUSH((a > b) ? 1u : 0u);
      break;
    }

    // 0x26 LE (signed)
    case 0x26: {
      CHECK_POP(2);
      int32_t b = (int32_t)POP(), a = (int32_t)POP();
      PUSH((a <= b) ? 1u : 0u);
      break;
    }

    // 0x27 GE (signed)
    case 0x27: {
      CHECK_POP(2);
      int32_t b = (int32_t)POP(), a = (int32_t)POP();
      PUSH((a >= b) ? 1u : 0u);
      break;
    }

    // 0x28 ADDI s16
    case 0x28: {
      int16_t imm = read_s16_le(code + ip);
      ip += 2;
      CHECK_POP(1);
      uint32_t x = POP();
      PUSH(x + (uint32_t)(int32_t)imm);
      break;
    }

    // 0x29 SUBI s16
    case 0x29: {
      int16_t imm = read_s16_le(code + ip);
      ip += 2;
      CHECK_POP(1);
      uint32_t x = POP();
      PUSH(x - (uint32_t)(int32_t)imm);
      break;
    }

    // 0x2A INC
    case 0x2A:
      CHECK_POP(1);
      stack[sp - 1] = stack[sp - 1] + 1u;
      break;

    // 0x2B DEC
    case 0x2B:
      CHECK_POP(1);
      stack[sp - 1] = stack[sp - 1] - 1u;
      break;

    // 0x2C MODS (signed)
    case 0x2C: {
      CHECK_POP(2);
      int32_t b = (int32_t)POP();
      int32_t a = (int32_t)POP();
      if (b == 0) TRAP(0xFFFF002Cu, "MODS divide by zero");
      PUSH((uint32_t)(a % b));
      break;
    }

    // 0x2D NOT
    case 0x2D:
      CHECK_POP(1);
      stack[sp - 1] = ~stack[sp - 1];
      break;

    // 0x30 LOAD_OFF s16 : base -> load32(base+imm)
    case 0x30: {
      int16_t imm = read_s16_le(code + ip);
      ip += 2;
      CHECK_POP(1);
      uint32_t base = POP();
      uint32_t addr = base + (uint32_t)(int32_t)imm;
      if (!MEM_ADDR32_OK(addr)) TRAP(0xFFFF0030u, "LOAD_OFF OOB");
      uint32_t v = read_u32_mem_le(mem, addr);
      PUSH(v);
      break;
    }

    // 0x31 STORE_OFF s16 : base value -> store32(base+imm, value)
    case 0x31: {
      int16_t imm = read_s16_le(code + ip);
      ip += 2;
      CHECK_POP(2);
      uint32_t val = POP();
      uint32_t base = POP();
      uint32_t addr = base + (uint32_t)(int32_t)imm;
      if (!MEM_ADDR32_OK(addr)) TRAP(0xFFFF0031u, "STORE_OFF OOB");
      write_u32_mem_le(mem, addr, val);
      break;
    }

    default:
      TRAP(0xFFFF00FEu, "unknown opcode");
  }

end:
  vm->ip = ip;
  vm->sp = sp;
  vm->fp = fp;

#undef MEM_ADDR32_OK
#undef MEM_ADDR_OK
#undef MEM_RANGE_OK
#undef POP
#undef PUSH
#undef CHECK_POP
#undef CHECK_PUSH
#undef TRAP
}

// ------------------------------ VM API (buffer-based) ------------------------------

static void vm_free(VM* vm) {
  if (!vm) return;
  if (vm->owns_container) free(vm->container);
  free(vm->code_starts);
  free(vm->mem);
  free(vm->stack);
  free(vm->out);
  memset(vm, 0, sizeof(*vm));
}

static int vm_init_from_container(VM* vm,
                                  uint8_t* container,
                                  size_t container_len,
                                  int owns_container,
                                  uint32_t stack_words) {
  if (!vm) return 0;
  memset(vm, 0, sizeof(*vm));

  ZvmHeader H;
  if (!read_zvm_header(container, container_len, &H)) return 0;

  const size_t need = 28ull + (size_t)H.code_size + (size_t)H.mem_init_size;
  if (need > container_len) return 0;
  if (H.mem_total_size < H.mem_init_size) return 0;
  if (H.entry_ip > H.code_size) return 0;
  if (stack_words == 0) return 0;

  vm->container = container;
  vm->container_len = container_len;
  vm->owns_container = owns_container;

  // Avoid copying code: point into the container blob.
  vm->code = container + 28;
  vm->code_size = H.code_size;

  vm->mem = (uint8_t*)calloc(H.mem_total_size ? H.mem_total_size : 1, 1);
  if (!vm->mem) die("out of memory (mem)");
  vm->mem_size = H.mem_total_size;
  memcpy(vm->mem, container + 28 + H.code_size, H.mem_init_size);

  vm->stack = (uint32_t*)calloc(stack_words, sizeof(uint32_t));
  if (!vm->stack) die("out of memory (stack)");
  vm->stack_cap = stack_words;

  vm->ip = H.entry_ip;
  vm->sp = 0;
  vm->fp = 0;

  const char* verr = NULL;
  if (!vm_verify_code(vm->code, vm->code_size, vm->ip, &vm->code_starts, &vm->code_starts_len, &verr)) {
    (void)verr;
    vm_free(vm);
    return 0;
  }

#if defined(__EMSCRIPTEN__)
  vm->stream_stdio = 0;
  vm->capture_output = 1;
#else
  vm->stream_stdio = 1;
  vm->capture_output = 0;
#endif

  return 1;
}

static int vm_run(VM* vm) {
  while (!vm->halted) step(vm);

  // Return 0 on clean halt/exit; 1 on trap.
  if (vm->trap_msg && strcmp(vm->trap_msg, "halt") != 0 && strcmp(vm->trap_msg, "exit") != 0) return 1;
  return 0;
}

// ------------------------------ Emscripten-friendly single-instance exports ------------------------------
// These are usable from JS by compiling with appropriate -s EXPORTED_FUNCTIONS.

static VM g_vm;
static int g_vm_inited = 0;

int svm_vm_load_from_buffer(uint8_t* zvm, uint32_t len, uint32_t stack_words) {
  if (g_vm_inited) { vm_free(&g_vm); g_vm_inited = 0; }
  if (!vm_init_from_container(&g_vm, zvm, (size_t)len, 0, stack_words)) return 0;
  g_vm_inited = 1;
  return 1;
}

int svm_vm_run_loaded(void) {
  if (!g_vm_inited) return -2;
  int trapped = vm_run(&g_vm);
  if (trapped) return -1;
  uint32_t rc = (g_vm.trap_msg && strcmp(g_vm.trap_msg, "exit") == 0) ? g_vm.trap_code : 0;
  return (int)(rc & 0xFFu);
}

uint32_t svm_vm_output_ptr(void) {
  if (!g_vm_inited || !g_vm.out) return 0u;
  return (uint32_t)(uintptr_t)g_vm.out;
}

uint32_t svm_vm_output_len(void) {
  if (!g_vm_inited) return 0u;
  return (uint32_t)g_vm.out_len;
}

void svm_vm_output_clear(void) {
  if (!g_vm_inited) return;
  out_clear(&g_vm);
}

uint32_t svm_vm_trap_code(void) {
  if (!g_vm_inited) return 0u;
  return g_vm.trap_code;
}

uint32_t svm_vm_ip(void) {
  if (!g_vm_inited) return 0u;
  return g_vm.ip;
}

void svm_vm_free_loaded(void) {
  if (!g_vm_inited) return;
  vm_free(&g_vm);
  g_vm_inited = 0;
}

// ------------------------------ Main ------------------------------

int main(int argc, char** argv) {
  if (argc < 2 || argc > 3) {
    fprintf(stderr, "usage: %s program.zvm [stack_words]\n", argv[0]);
    return 2;
  }

  const char* path = argv[1];
  uint32_t stack_words = 65536u;
  if (argc == 3) {
    long long sw = atoll(argv[2]);
    if (sw <= 0 || sw > 0x7fffffffLL) die("invalid stack_words");
    stack_words = (uint32_t)sw;
  }

  size_t file_len = 0;
  uint8_t* file = read_file(path, &file_len);

  VM vm;
  if (!vm_init_from_container(&vm, file, file_len, 1, stack_words)) {
    fprintf(stderr, "error: invalid or truncated ZVM1 file: %s\n", path);
    free(file);
    return 1;
  }

  // Native CLI behavior: stream to stdio; do not capture output.
  vm.stream_stdio = 1;
  vm.capture_output = 0;

  (void)vm_run(&vm);

  if (vm.trap_msg && strcmp(vm.trap_msg, "halt") != 0 && strcmp(vm.trap_msg, "exit") != 0) {
    fprintf(stderr, "\nVM TRAP: code=0x%08X ip=0x%08X (%s)\n", vm.trap_code, vm.ip, vm.trap_msg);
    vm_free(&vm);
    return 1;
  }

  uint32_t rc = (vm.trap_msg && strcmp(vm.trap_msg, "exit") == 0) ? vm.trap_code : 0;
  vm_free(&vm);
  return (int)(rc & 0xFFu);
}
