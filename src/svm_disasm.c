/*
StackVM-32 Disassembler (v1.1) with Auto-Labels
-----------------------------------------------
Disassembles StackVM-32 bytecode (.code) to readable assembly-like text,
and auto-generates labels for branch/call targets.

Build:
  cc -O2 -std=c11 -Wall -Wextra -o svm_disasm svm_disasm.c

Usage:
  ./svm_disasm program.zvm

Output:
  - Generates labels like L000000A0 for any addr32 target that falls within code.
  - Prints labels on their own line before the instruction at that address.
  - Prints branch/call operands as labels when possible.
  - Still prints raw bytes and instruction addresses to help debugging.

Notes:
  - Symbol-free: labels are synthetic.
  - Disassembler does not validate stack effects; it just decodes byte stream.
  - If it encounters an unknown opcode, it prints "DB 0x??" and continues.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>

typedef enum { OP_NONE, OP_U8, OP_U16, OP_S16, OP_U32, OP_ADDR32 } OpArg;

typedef struct {
  const char* name;
  uint8_t opcode;
  OpArg arg;
} Instr;

static const Instr INSTRS[] = {
  // Control
  {"NOP", 0x00, OP_NONE},
  {"HALT", 0x01, OP_NONE},
  {"SYSCALL", 0x02, OP_U8},
  {"TRAP", 0x03, OP_U16},

  // Branch
  {"JMP", 0x04, OP_ADDR32},
  {"JZ",  0x05, OP_ADDR32},
  {"JNZ", 0x06, OP_ADDR32},

  // Stack
  {"PUSHI", 0x07, OP_U32},
  {"POP",   0x08, OP_NONE},
  {"DUP",   0x09, OP_NONE},
  {"DUP2",  0x0A, OP_NONE},
  {"SWAP",  0x0B, OP_NONE},
  {"ROT",   0x0C, OP_NONE},
  {"OVER",  0x0D, OP_NONE},

  // Calls & frames
  {"CALL",  0x0E, OP_ADDR32},
  {"RET",   0x0F, OP_U8},
  {"ENTER", 0x10, OP_U16},
  {"LEAVE", 0x11, OP_NONE},
  {"LDFP",  0x12, OP_S16},
  {"STFP",  0x13, OP_S16},

  // Memory
  {"LOAD32",  0x14, OP_NONE},
  {"STORE32", 0x15, OP_NONE},
  {"LOAD8U",  0x16, OP_NONE},
  {"STORE8",  0x17, OP_NONE},
  {"MEMCPY",  0x18, OP_NONE},

  // Arithmetic & bitwise
  {"ADD",  0x19, OP_NONE},
  {"SUB",  0x1A, OP_NONE},
  {"MUL",  0x1B, OP_NONE},
  {"DIVS", 0x1C, OP_NONE},
  {"NEG",  0x1D, OP_NONE},
  {"AND",  0x1E, OP_NONE},
  {"OR",   0x1F, OP_NONE},
  {"XOR",  0x20, OP_NONE},
  {"SHL",  0x21, OP_NONE},
  {"SHR",  0x22, OP_NONE},

  // Compare
  {"EQ", 0x23, OP_NONE},
  {"LT", 0x24, OP_NONE},
  {"GT", 0x25, OP_NONE},
  {"LE", 0x26, OP_NONE},
  {"GE", 0x27, OP_NONE},

  // Convenience
  {"ADDI",     0x28, OP_S16},
  {"SUBI",     0x29, OP_S16},
  {"INC",      0x2A, OP_NONE},
  {"DEC",      0x2B, OP_NONE},
  {"MODS",     0x2C, OP_NONE},
  {"NOT",      0x2D, OP_NONE},
  {"CALLI",    0x2E, OP_NONE},
  {"TAILCALL", 0x2F, OP_ADDR32},
  {"LOAD_OFF",  0x30, OP_S16},
  {"STORE_OFF", 0x31, OP_S16},
};

static const size_t INSTRS_N = sizeof(INSTRS)/sizeof(INSTRS[0]);

static const Instr* find_instr(uint8_t opcode) {
  for (size_t i = 0; i < INSTRS_N; i++) {
    if (INSTRS[i].opcode == opcode) return &INSTRS[i];
  }
  return NULL;
}

#if defined(__clang__) || defined(__GNUC__)
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN
#endif

static NORETURN void die(const char* msg) {
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

static uint16_t u16le(const uint8_t* p) {
  return (uint16_t)(p[0] | ((uint16_t)p[1] << 8));
}

static int16_t s16le(const uint8_t* p) {
  return (int16_t)u16le(p);
}

static uint32_t u32le(const uint8_t* p) {
  return (uint32_t)p[0]
      | ((uint32_t)p[1] << 8)
      | ((uint32_t)p[2] << 16)
      | ((uint32_t)p[3] << 24);
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
  out->code_size      = u32le(buf + 8);
  out->mem_init_size  = u32le(buf + 12);
  out->mem_total_size = u32le(buf + 16);
  out->entry_ip       = u32le(buf + 20);
  return 1;
}

static int instr_len(OpArg arg) {
  switch (arg) {
    case OP_NONE:   return 1;
    case OP_U8:     return 2;
    case OP_U16:    return 3;
    case OP_S16:    return 3;
    case OP_U32:    return 5;
    case OP_ADDR32: return 5;
    default:        return 1;
  }
}

static void print_bytes(const uint8_t* code, size_t off, size_t n, size_t code_len) {
  for (size_t i = 0; i < 6; i++) {
    if (i < n && off + i < code_len) printf("%02X ", code[off + i]);
    else printf("   ");
  }
}

static int is_addr32_op(uint8_t opcode) {
  // All ops that take addr32:
  // JMP (0x04), JZ (0x05), JNZ (0x06), CALL (0x0E), TAILCALL (0x2F)
  return opcode == 0x04 || opcode == 0x05 || opcode == 0x06 || opcode == 0x0E || opcode == 0x2F;
}

// Label table: map address -> label id (dense map with a byte array for simplicity)
typedef struct {
  uint8_t* is_label; // length code_len, is_label[addr]=1 if label
} Labels;

static void labels_init(Labels* L, size_t code_len) {
  L->is_label = (uint8_t*)calloc(code_len ? code_len : 1, 1);
  if (!L->is_label) die("out of memory (labels)");
}

static void labels_free(Labels* L) {
  free(L->is_label);
  L->is_label = NULL;
}

static void labels_collect(Labels* L, const uint8_t* code, size_t code_len) {
  // Always label entrypoint at 0
  if (code_len > 0) L->is_label[0] = 1;

  size_t ip = 0;
  while (ip < code_len) {
    uint8_t op = code[ip];
    const Instr* ins = find_instr(op);

    if (!ins) {
      ip += 1;
      continue;
    }

    int len = instr_len(ins->arg);
    if (ip + (size_t)len > code_len) break; // truncated

    if (ins->arg == OP_ADDR32 && is_addr32_op(op)) {
      uint32_t a = u32le(code + ip + 1);
      if (a < code_len) L->is_label[a] = 1;
    }

    ip += (size_t)len;
  }
}

static void print_label_name(uint32_t addr) {
  // Synthetic label format
  printf("L%08X", addr);
}

int main(int argc, char** argv) {
  if (argc != 2) {
    fprintf(stderr, "usage: %s program.zvm\n", argv[0]);
    return 2;
  }

  size_t file_len = 0;
  uint8_t* file = read_file(argv[1], &file_len);

  ZvmHeader H;
  if (!read_zvm_header(file, file_len, &H)) {
    fprintf(stderr, "error: not a valid ZVM1 file: %s\n", argv[1]);
    free(file);
    return 1;
  }

  const size_t need = 28ull + (size_t)H.code_size + (size_t)H.mem_init_size;
  if (need > file_len) {
    fprintf(stderr, "error: truncated ZVM1 file: %s\n", argv[1]);
    free(file);
    return 1;
  }

  const uint8_t* code = file + 28;
  const size_t code_len = (size_t)H.code_size;

  Labels labs;
  labels_init(&labs, code_len);
  labels_collect(&labs, code, code_len);

  printf(".code\n");

  size_t ip = 0;
  while (ip < code_len) {
    if (labs.is_label[ip]) {
      print_label_name((uint32_t)ip);
      printf(":\n");
    }

    uint8_t op = code[ip];
    const Instr* ins = find_instr(op);

    if (!ins) {
      printf("  %08zX  ", ip);
      print_bytes(code, ip, 1, code_len);
      printf("DB 0x%02X\n", op);
      ip += 1;
      continue;
    }

    int len = instr_len(ins->arg);
    size_t avail = (ip + (size_t)len <= code_len) ? (size_t)len : (code_len - ip);

    printf("  %08zX  ", ip);
    print_bytes(code, ip, avail, code_len);

    printf("%-9s", ins->name);

    if (ins->arg == OP_NONE) {
      printf("\n");
      ip += 1;
      continue;
    }

    if (ip + (size_t)len > code_len) {
      printf(" ; <truncated>\n");
      break;
    }

    const uint8_t* imm = code + ip + 1;

    switch (ins->arg) {
      case OP_U8: {
        uint8_t v = imm[0];
        printf(" %u", (unsigned)v);
        break;
      }
      case OP_U16: {
        uint16_t v = u16le(imm);
        printf(" %u", (unsigned)v);
        break;
      }
      case OP_S16: {
        int16_t v = s16le(imm);
        printf(" %d", (int)v);
        break;
      }
      case OP_U32: {
        uint32_t v = u32le(imm);
        printf(" %u ; 0x%08X", v, v);
        break;
      }
      case OP_ADDR32: {
        uint32_t a = u32le(imm);
        printf(" ");
        if (a < code_len && labs.is_label[a]) {
          print_label_name(a);
          printf(" ; 0x%08X", a);
        } else {
          printf("0x%08X", a);
        }
        break;
      }
      default:
        break;
    }

    printf("\n");
    ip += (size_t)len;
  }

  labels_free(&labs);
  free(file);
  return 0;
}
