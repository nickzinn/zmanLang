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
  uint8_t* code;
  uint32_t code_size;

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
} VM;

// ------------------------------ Helpers ------------------------------

static void die(const char* msg) {
  fprintf(stderr, "error: %s\n", msg);
  exit(1);
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
  uint8_t* buf = (uint8_t*)malloc((size_t)n ? (size_t)n : 1);
  if (!buf) die("out of memory");
  size_t rd = fread(buf, 1, (size_t)n, f);
  fclose(f);
  if (rd != (size_t)n) die("file read failed");
  *out_len = (size_t)n;
  return buf;
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

static int check_ip(VM* vm, uint32_t need) {
  // need bytes available starting at ip
  if (vm->ip + need > vm->code_size) {
    trap(vm, 0xFFFF0001u, "IP out of range");
    return 0;
  }
  return 1;
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

static int mem_addr_ok(VM* vm, uint32_t addr) {
  return addr < vm->mem_size;
}

static int mem_addr32_ok(VM* vm, uint32_t addr) {
  return addr + 4u <= vm->mem_size;
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
      printf("%u", x);
      fflush(stdout);
      return;
    }
    case 2: { // print_i32(x)
      if (!check_stack_pop(vm, 1)) return;
      int32_t x = (int32_t)pop(vm);
      printf("%d", x);
      fflush(stdout);
      return;
    }
    case 3: { // putchar(ch)
      if (!check_stack_pop(vm, 1)) return;
      uint32_t ch = pop(vm);
      fputc((int)(ch & 0xFFu), stdout);
      fflush(stdout);
      return;
    }
    case 4: { // write(ptr,len)
      if (!check_stack_pop(vm, 2)) return;
      uint32_t len = pop(vm);
      uint32_t ptr = pop(vm);
      if (!mem_range_ok(vm, ptr, len)) { trap(vm, 0xFFFF0100u, "syscall write OOB"); return; }
      if (len) fwrite(vm->mem + ptr, 1, len, stdout);
      fflush(stdout);
      return;
    }
    case 5: { // read(ptr,len) -> n
      if (!check_stack_pop(vm, 2)) return;
      uint32_t len = pop(vm);
      uint32_t ptr = pop(vm);
      if (!mem_range_ok(vm, ptr, len)) { trap(vm, 0xFFFF0101u, "syscall read OOB"); return; }
      size_t n = 0;
      if (len) n = fread(vm->mem + ptr, 1, len, stdin);
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

static void step(VM* vm) {
  if (!check_ip(vm, 1)) return;
  uint8_t op = vm->code[vm->ip++];

  switch (op) {
    // 0x00 NOP
    case 0x00: return;

    // 0x01 HALT
    case 0x01:
      vm->halted = 1;
      vm->trap_code = 0;
      vm->trap_msg = "halt";
      return;

    // 0x02 SYSCALL u8
    case 0x02: {
      if (!check_ip(vm, 1)) return;
      uint8_t id = vm->code[vm->ip++];
      do_syscall(vm, id);
      return;
    }

    // 0x03 TRAP u16
    case 0x03: {
      if (!check_ip(vm, 2)) return;
      uint16_t tc = read_u16_le(vm->code + vm->ip);
      vm->ip += 2;
      trap(vm, (uint32_t)tc, "TRAP");
      return;
    }

    // 0x04 JMP addr32
    case 0x04: {
      if (!check_ip(vm, 4)) return;
      uint32_t a = read_u32_le(vm->code + vm->ip);
      vm->ip = a;
      if (vm->ip >= vm->code_size) trap(vm, 0xFFFF0004u, "JMP to invalid address");
      return;
    }

    // 0x05 JZ addr32
    case 0x05: {
      if (!check_ip(vm, 4)) return;
      uint32_t a = read_u32_le(vm->code + vm->ip);
      vm->ip += 4;
      if (!check_stack_pop(vm, 1)) return;
      uint32_t c = pop(vm);
      if (c == 0) {
        vm->ip = a;
        if (vm->ip >= vm->code_size) trap(vm, 0xFFFF0005u, "JZ to invalid address");
      }
      return;
    }

    // 0x06 JNZ addr32
    case 0x06: {
      if (!check_ip(vm, 4)) return;
      uint32_t a = read_u32_le(vm->code + vm->ip);
      vm->ip += 4;
      if (!check_stack_pop(vm, 1)) return;
      uint32_t c = pop(vm);
      if (c != 0) {
        vm->ip = a;
        if (vm->ip >= vm->code_size) trap(vm, 0xFFFF0006u, "JNZ to invalid address");
      }
      return;
    }

    // 0x07 PUSHI u32
    case 0x07: {
      if (!check_ip(vm, 4)) return;
      uint32_t imm = read_u32_le(vm->code + vm->ip);
      vm->ip += 4;
      if (!check_stack_push(vm, 1)) return;
      push(vm, imm);
      return;
    }

    // 0x08 POP
    case 0x08:
      if (!check_stack_pop(vm, 1)) return;
      vm->sp--;
      return;

    // 0x09 DUP
    case 0x09:
      if (!check_stack_pop(vm, 1) || !check_stack_push(vm, 1)) return;
      push(vm, vm->stack[vm->sp - 1]);
      return;

    // 0x0A DUP2
    case 0x0A:
      if (!check_stack_pop(vm, 2) || !check_stack_push(vm, 2)) return;
      {
        uint32_t a = vm->stack[vm->sp - 2];
        uint32_t b = vm->stack[vm->sp - 1];
        push(vm, a);
        push(vm, b);
      }
      return;

    // 0x0B SWAP
    case 0x0B:
      if (!check_stack_pop(vm, 2)) return;
      {
        uint32_t b = pop(vm);
        uint32_t a = pop(vm);
        push(vm, b);
        push(vm, a);
      }
      return;

    // 0x0C ROT: a b c -> b c a
    case 0x0C:
      if (!check_stack_pop(vm, 3)) return;
      {
        uint32_t c = pop(vm);
        uint32_t b = pop(vm);
        uint32_t a = pop(vm);
        push(vm, b);
        push(vm, c);
        push(vm, a);
      }
      return;

    // 0x0D OVER: a b -> a b a
    case 0x0D:
      if (!check_stack_pop(vm, 2) || !check_stack_push(vm, 1)) return;
      push(vm, vm->stack[vm->sp - 2]);
      return;

    // 0x0E CALL addr32
    case 0x0E: {
      if (!check_ip(vm, 4)) return;
      uint32_t target = read_u32_le(vm->code + vm->ip);
      vm->ip += 4;
      if (target >= vm->code_size) { trap(vm, 0xFFFF000Eu, "CALL to invalid address"); return; }
      if (!check_stack_push(vm, 2)) return;
      uint32_t ret_ip = vm->ip;
      push(vm, ret_ip);
      push(vm, vm->fp);
      vm->fp = vm->sp;
      vm->ip = target;
      return;
    }

    // 0x2E CALLI (addr on stack)
    case 0x2E: {
      if (!check_stack_pop(vm, 1)) return;
      uint32_t target = pop(vm);
      if (target >= vm->code_size) { trap(vm, 0xFFFF002Eu, "CALLI to invalid address"); return; }
      if (!check_stack_push(vm, 2)) return;
      uint32_t ret_ip = vm->ip;
      push(vm, ret_ip);
      push(vm, vm->fp);
      vm->fp = vm->sp;
      vm->ip = target;
      return;
    }

    // 0x10 ENTER u16
    case 0x10: {
      if (!check_ip(vm, 2)) return;
      uint16_t nlocals = read_u16_le(vm->code + vm->ip);
      vm->ip += 2;
      if (!check_stack_push(vm, nlocals)) return;
      // deterministic: zero locals
      for (uint16_t i = 0; i < nlocals; i++) push(vm, 0);
      return;
    }

    // 0x11 LEAVE
    case 0x11:
      if (vm->fp > vm->sp) { trap(vm, 0xFFFF0011u, "LEAVE with corrupted fp"); return; }
      vm->sp = vm->fp;
      return;

    // 0x12 LDFP s16
    case 0x12: {
      if (!check_ip(vm, 2)) return;
      int16_t off = read_s16_le(vm->code + vm->ip);
      vm->ip += 2;
      int64_t idx = (int64_t)vm->fp + (int64_t)off;
      if (idx < 0 || idx >= (int64_t)vm->sp) { trap(vm, 0xFFFF0012u, "LDFP index out of range"); return; }
      if (!check_stack_push(vm, 1)) return;
      push(vm, vm->stack[(uint32_t)idx]);
      return;
    }

    // 0x13 STFP s16
    case 0x13: {
      if (!check_ip(vm, 2)) return;
      int16_t off = read_s16_le(vm->code + vm->ip);
      vm->ip += 2;
      if (!check_stack_pop(vm, 1)) return;
      uint32_t val = pop(vm);
      int64_t idx = (int64_t)vm->fp + (int64_t)off;
      if (idx < 0 || idx >= (int64_t)vm->sp) { trap(vm, 0xFFFF0013u, "STFP index out of range"); return; }
      vm->stack[(uint32_t)idx] = val;
      return;
    }

    // 0x0F RET u8
    case 0x0F: {
      if (!check_ip(vm, 1)) return;
      uint8_t argc = vm->code[vm->ip++];
      if (!check_stack_pop(vm, 1)) return;
      uint32_t rv = pop(vm);

      if (vm->fp > vm->sp) { trap(vm, 0xFFFF000Fu, "RET with corrupted fp"); return; }
      vm->sp = vm->fp;

      // Need old_fp and ret_ip on stack
      if (!check_stack_pop(vm, 2)) return;
      uint32_t old_fp = pop(vm);
      uint32_t ret_ip = pop(vm);

      if (vm->sp < argc) { trap(vm, 0xFFFF000Fu, "RET argc underflow"); return; }
      vm->sp -= argc;

      if (!check_stack_push(vm, 1)) return;
      push(vm, rv);

      vm->fp = old_fp;
      vm->ip = ret_ip;
      if (vm->ip > vm->code_size) { trap(vm, 0xFFFF000Fu, "RET to invalid ip"); return; }
      return;
    }

    // 0x2F TAILCALL addr32
    case 0x2F: {
      if (!check_ip(vm, 4)) return;
      uint32_t target = read_u32_le(vm->code + vm->ip);
      vm->ip = target;
      if (vm->ip >= vm->code_size) trap(vm, 0xFFFF002Fu, "TAILCALL to invalid address");
      return;
    }

    // 0x14 LOAD32
    case 0x14: {
      if (!check_stack_pop(vm, 1)) return;
      uint32_t addr = pop(vm);
      if (!mem_addr32_ok(vm, addr)) { trap(vm, 0xFFFF0014u, "LOAD32 OOB"); return; }
      uint32_t v = read_u32_mem_le(vm->mem, addr);
      if (!check_stack_push(vm, 1)) return;
      push(vm, v);
      return;
    }

    // 0x15 STORE32
    case 0x15: {
      if (!check_stack_pop(vm, 2)) return;
      uint32_t val = pop(vm);
      uint32_t addr = pop(vm);
      if (!mem_addr32_ok(vm, addr)) { trap(vm, 0xFFFF0015u, "STORE32 OOB"); return; }
      write_u32_mem_le(vm->mem, addr, val);
      return;
    }

    // 0x16 LOAD8U
    case 0x16: {
      if (!check_stack_pop(vm, 1)) return;
      uint32_t addr = pop(vm);
      if (!mem_addr_ok(vm, addr)) { trap(vm, 0xFFFF0016u, "LOAD8U OOB"); return; }
      if (!check_stack_push(vm, 1)) return;
      push(vm, (uint32_t)vm->mem[addr]);
      return;
    }

    // 0x17 STORE8
    case 0x17: {
      if (!check_stack_pop(vm, 2)) return;
      uint32_t val = pop(vm);
      uint32_t addr = pop(vm);
      if (!mem_addr_ok(vm, addr)) { trap(vm, 0xFFFF0017u, "STORE8 OOB"); return; }
      vm->mem[addr] = (uint8_t)(val & 0xFFu);
      return;
    }

    // 0x18 MEMCPY (memmove semantics): dest src len ->
    case 0x18: {
      if (!check_stack_pop(vm, 3)) return;
      uint32_t len  = pop(vm);
      uint32_t src  = pop(vm);
      uint32_t dest = pop(vm);
      if (!mem_range_ok(vm, src, len) || !mem_range_ok(vm, dest, len)) {
        trap(vm, 0xFFFF0018u, "MEMCPY OOB");
        return;
      }
      memmove(vm->mem + dest, vm->mem + src, (size_t)len);
      return;
    }

    // 0x19 ADD
    case 0x19: {
      if (!check_stack_pop(vm, 2)) return;
      uint32_t b = pop(vm), a = pop(vm);
      push(vm, a + b);
      return;
    }

    // 0x1A SUB
    case 0x1A: {
      if (!check_stack_pop(vm, 2)) return;
      uint32_t b = pop(vm), a = pop(vm);
      push(vm, a - b);
      return;
    }

    // 0x1B MUL
    case 0x1B: {
      if (!check_stack_pop(vm, 2)) return;
      uint32_t b = pop(vm), a = pop(vm);
      push(vm, a * b);
      return;
    }

    // 0x1C DIVS (signed)
    case 0x1C: {
      if (!check_stack_pop(vm, 2)) return;
      int32_t b = (int32_t)pop(vm);
      int32_t a = (int32_t)pop(vm);
      if (b == 0) { trap(vm, 0xFFFF001Cu, "DIVS divide by zero"); return; }
      // optional determinism trap: INT_MIN / -1
      if (a == (int32_t)0x80000000 && b == -1) { trap(vm, 0xFFFF001Cu, "DIVS overflow"); return; }
      push(vm, (uint32_t)(a / b));
      return;
    }

    // 0x1D NEG
    case 0x1D: {
      if (!check_stack_pop(vm, 1)) return;
      uint32_t a = pop(vm);
      push(vm, (uint32_t)(-(int32_t)a));
      return;
    }

    // 0x1E AND
    case 0x1E: {
      if (!check_stack_pop(vm, 2)) return;
      uint32_t b = pop(vm), a = pop(vm);
      push(vm, a & b);
      return;
    }

    // 0x1F OR
    case 0x1F: {
      if (!check_stack_pop(vm, 2)) return;
      uint32_t b = pop(vm), a = pop(vm);
      push(vm, a | b);
      return;
    }

    // 0x20 XOR
    case 0x20: {
      if (!check_stack_pop(vm, 2)) return;
      uint32_t b = pop(vm), a = pop(vm);
      push(vm, a ^ b);
      return;
    }

    // 0x21 SHL
    case 0x21: {
      if (!check_stack_pop(vm, 2)) return;
      uint32_t sh = pop(vm), v = pop(vm);
      push(vm, v << mask_shift(sh));
      return;
    }

    // 0x22 SHR (logical)
    case 0x22: {
      if (!check_stack_pop(vm, 2)) return;
      uint32_t sh = pop(vm), v = pop(vm);
      push(vm, v >> mask_shift(sh));
      return;
    }

    // 0x23 EQ
    case 0x23: {
      if (!check_stack_pop(vm, 2)) return;
      uint32_t b = pop(vm), a = pop(vm);
      push(vm, (a == b) ? 1u : 0u);
      return;
    }

    // 0x24 LT (signed)
    case 0x24: {
      if (!check_stack_pop(vm, 2)) return;
      int32_t b = (int32_t)pop(vm), a = (int32_t)pop(vm);
      push(vm, (a < b) ? 1u : 0u);
      return;
    }

    // 0x25 GT (signed)
    case 0x25: {
      if (!check_stack_pop(vm, 2)) return;
      int32_t b = (int32_t)pop(vm), a = (int32_t)pop(vm);
      push(vm, (a > b) ? 1u : 0u);
      return;
    }

    // 0x26 LE (signed)
    case 0x26: {
      if (!check_stack_pop(vm, 2)) return;
      int32_t b = (int32_t)pop(vm), a = (int32_t)pop(vm);
      push(vm, (a <= b) ? 1u : 0u);
      return;
    }

    // 0x27 GE (signed)
    case 0x27: {
      if (!check_stack_pop(vm, 2)) return;
      int32_t b = (int32_t)pop(vm), a = (int32_t)pop(vm);
      push(vm, (a >= b) ? 1u : 0u);
      return;
    }

    // 0x28 ADDI s16
    case 0x28: {
      if (!check_ip(vm, 2)) return;
      int16_t imm = read_s16_le(vm->code + vm->ip);
      vm->ip += 2;
      if (!check_stack_pop(vm, 1)) return;
      uint32_t x = pop(vm);
      push(vm, x + (uint32_t)(int32_t)imm);
      return;
    }

    // 0x29 SUBI s16
    case 0x29: {
      if (!check_ip(vm, 2)) return;
      int16_t imm = read_s16_le(vm->code + vm->ip);
      vm->ip += 2;
      if (!check_stack_pop(vm, 1)) return;
      uint32_t x = pop(vm);
      push(vm, x - (uint32_t)(int32_t)imm);
      return;
    }

    // 0x2A INC
    case 0x2A:
      if (!check_stack_pop(vm, 1)) return;
      vm->stack[vm->sp - 1] = vm->stack[vm->sp - 1] + 1u;
      return;

    // 0x2B DEC
    case 0x2B:
      if (!check_stack_pop(vm, 1)) return;
      vm->stack[vm->sp - 1] = vm->stack[vm->sp - 1] - 1u;
      return;

    // 0x2C MODS (signed)
    case 0x2C: {
      if (!check_stack_pop(vm, 2)) return;
      int32_t b = (int32_t)pop(vm);
      int32_t a = (int32_t)pop(vm);
      if (b == 0) { trap(vm, 0xFFFF002Cu, "MODS divide by zero"); return; }
      push(vm, (uint32_t)(a % b));
      return;
    }

    // 0x2D NOT
    case 0x2D:
      if (!check_stack_pop(vm, 1)) return;
      vm->stack[vm->sp - 1] = ~vm->stack[vm->sp - 1];
      return;

    // 0x30 LOAD_OFF s16 : base -> load32(base+imm)
    case 0x30: {
      if (!check_ip(vm, 2)) return;
      int16_t imm = read_s16_le(vm->code + vm->ip);
      vm->ip += 2;
      if (!check_stack_pop(vm, 1)) return;
      uint32_t base = pop(vm);
      uint32_t addr = base + (uint32_t)(int32_t)imm;
      if (!mem_addr32_ok(vm, addr)) { trap(vm, 0xFFFF0030u, "LOAD_OFF OOB"); return; }
      uint32_t v = read_u32_mem_le(vm->mem, addr);
      push(vm, v);
      return;
    }

    // 0x31 STORE_OFF s16 : base value -> store32(base+imm, value)
    case 0x31: {
      if (!check_ip(vm, 2)) return;
      int16_t imm = read_s16_le(vm->code + vm->ip);
      vm->ip += 2;
      if (!check_stack_pop(vm, 2)) return;
      uint32_t val = pop(vm);
      uint32_t base = pop(vm);
      uint32_t addr = base + (uint32_t)(int32_t)imm;
      if (!mem_addr32_ok(vm, addr)) { trap(vm, 0xFFFF0031u, "STORE_OFF OOB"); return; }
      write_u32_mem_le(vm->mem, addr, val);
      return;
    }

    default:
      trap(vm, 0xFFFF00FEu, "unknown opcode");
      return;
  }
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

  ZvmHeader H;
  if (!read_zvm_header(file, file_len, &H)) {
    fprintf(stderr, "error: not a valid ZVM1 file: %s\n", path);
    free(file);
    return 1;
  }

  const size_t need = 28ull + (size_t)H.code_size + (size_t)H.mem_init_size;
  if (need > file_len) {
    fprintf(stderr, "error: truncated ZVM1 file: %s\n", path);
    free(file);
    return 1;
  }
  if (H.mem_total_size < H.mem_init_size) {
    fprintf(stderr, "error: invalid ZVM1 header (mem_total < mem_init): %s\n", path);
    free(file);
    return 1;
  }
  if (H.entry_ip > H.code_size) {
    fprintf(stderr, "error: invalid ZVM1 header (entry_ip out of range): %s\n", path);
    free(file);
    return 1;
  }

  uint8_t* code = (uint8_t*)malloc(H.code_size ? H.code_size : 1);
  if (!code) die("out of memory (code)");
  memcpy(code, file + 28, H.code_size);

  uint8_t* mem = (uint8_t*)calloc(H.mem_total_size ? H.mem_total_size : 1, 1);
  if (!mem) die("out of memory (mem)");
  memcpy(mem, file + 28 + H.code_size, H.mem_init_size);

  free(file);

  VM vm;
  memset(&vm, 0, sizeof(vm));
  vm.code = code;
  vm.code_size = H.code_size;
  vm.mem = mem;
  vm.mem_size = H.mem_total_size;
  vm.stack = (uint32_t*)calloc(stack_words, sizeof(uint32_t));
  if (!vm.stack) die("out of memory (stack)");
  vm.stack_cap = stack_words;

  vm.ip = H.entry_ip;
  vm.sp = 0;
  vm.fp = 0;

  while (!vm.halted) step(&vm);

  if (vm.trap_msg && strcmp(vm.trap_msg, "halt") != 0 && strcmp(vm.trap_msg, "exit") != 0) {
    fprintf(stderr, "\nVM TRAP: code=0x%08X ip=0x%08X (%s)\n", vm.trap_code, vm.ip, vm.trap_msg);
    free(vm.code);
    free(vm.mem);
    free(vm.stack);
    return 1;
  }

  uint32_t rc = (vm.trap_msg && strcmp(vm.trap_msg, "exit") == 0) ? vm.trap_code : 0;

  free(vm.code);
  free(vm.mem);
  free(vm.stack);
  return (int)(rc & 0xFFu);
}
