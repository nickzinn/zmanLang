StackVM-32 Assembler Syntax (v1)

1) File structure

An assembly file is a sequence of:
    •    Directives (start with .)
    •    Labels (name:)
    •    Instructions
    •    Comments

Recommended layout:

.module "demo"

.code
.entry main

main:
  ; code here

.data
  ; optional: initial linear memory bytes

.end

Only .code is required.

⸻

2) Lexical rules

2.1 Whitespace
    •    Spaces/tabs separate tokens.
    •    Newlines separate statements.
    •    Commas are optional separators for operands.

2.2 Comments
    •    ; starts a comment until end-of-line.

PUSHI 10      ; push ten

2.3 Identifiers
    •    [A-Za-z_][A-Za-z0-9_]*

Examples: main, _loop1, str_hello

2.4 Numeric literals (all assemble to integers)
    •    Decimal: 123
    •    Hex: 0x7B
    •    Binary: 0b1111011
    •    Negative: -42, -0x2A

Assembler range-checks based on operand width:
    •    u8  : 0..255
    •    u16 : 0..65535
    •    s16 : -32768..32767
    •    u32 : 0..4294967295 (accept negatives and wrap only if you explicitly choose to; I recommend range-check and require U32(...) if you want wrap)

⸻

3) Labels and symbol resolution

3.1 Code labels

A label defines a code address (byte offset in .code) for use with addr32 operands:

main:
  PUSHI 1
  HALT

3.2 Data labels

Labels in .data define memory addresses (byte offset in linear memory initial image):

.data
hello:
  .ascii "hi", 0

Then in code:

  PUSHI hello   ; pushes address of data label (u32)

3.3 Forward references

Allowed. Assembler emits a fixup and patches later.

⸻

4) Expressions

Operands can be simple expressions:
    •    label
    •    label + constant
    •    label - constant
    •    constant + constant

Examples:

JMP loop
PUSHI hello + 2
ADDI -8
LDFP -4
LOAD_OFF 12

Keep it deliberately small: no parentheses, no precedence rules beyond left-to-right, unless you want them.

⸻

5) Sections and directives

5.1 .code / .data

Switch current output section.

.code
.data

5.2 .org expr

Set the current output position within the current section.
    •    In .code, sets the code offset (in bytes)
    •    In .data, sets the memory offset (in bytes)

.data
.org 0x1000
buf:
  .zero 256

5.3 .align n

Align current section offset to n bytes (n is power of 2).

.code
.align 4

5.4 Data emission directives (in .data, optionally allowed in .code too)
    •    .byte b0, b1, ...  (u8 each)
    •    .word w0, w1, ...  (u32 each, little-endian)
    •    .ascii "text"      (raw bytes, no terminator)
    •    .asciz "text"      (bytes + trailing 0)
    •    .zero n            (emit n zero bytes)

Examples:

.data
hello: .asciz "Hello, world!"
nums:  .word 1, 2, 3, 0x12345678
pad:   .zero 16

5.5 .const name = expr

Define a symbol constant (does not allocate space):

.const SYS_WRITE = 1
.const STACK_SLOTS = 1024

5.6 .entry expr

Set the program entry point (initial IP) written into the ZVM1 header’s `EntryIP` field.

Syntax:
    .entry label
    .entry label + constant
    .entry constant

Notes:
        •    The entry point is a byte offset into the assembled `.code` section.
        •    If `expr` uses a label, it must refer to a code label (same kind as used by `JMP`/`CALL`).
        •    The resolved entry IP must be in range: `0 <= EntryIP < CodeSize`.
        •    The assembler should reject negative values.
        •    Only one `.entry` directive is allowed per file.

Example:

.code
.entry main

main:
    HALT

5.7 .module "name" / .end

Optional metadata.

5.8 .memtotal expr

Set the ZVM1 header’s `MemTotalSize` field (total linear memory size at runtime).

This lets programs reserve additional zero-initialized memory beyond what `.data`
emits (useful for heap-based runtimes).

Syntax:
    .memtotal constant
    .memtotal CONST_NAME

Notes:
    •    `MemTotalSize` must be `>= MemInitSize` (i.e., at least the highest byte written by `.data`).
    •    Implementations may provide a default when `.memtotal` is omitted (this project defaults to at least 64 KiB).
    •    In this assembler implementation, the argument must be an immediate or a `.const` symbol (not a label).

⸻

6) Instruction syntax

6.1 General form
    •    MNEMONIC (uppercase recommended, case-insensitive ok)
    •    Operands separated by spaces and/or commas

PUSHI 10
JMP loop
SYSCALL 1
LDFP -4

6.2 Operand types (match your VM spec)
    •    addr32: code label or expression (patched to absolute u32)
    •    u32: constant or data label (address)
    •    u16, u8, s16: range-checked numbers/expressions

⸻

7) Mnemonics (exactly match VM spec)

Control

NOP
HALT
SYSCALL u8
TRAP u16

Branch (absolute addr32)

JMP  label
JZ   label
JNZ  label

Stack

PUSHI u32
POP
DUP
DUP2
SWAP
ROT
OVER

Calls & frames

CALL     label
CALLI
RET      u8        ; argc
ENTER    u16       ; nlocals
LEAVE
LDFP     s16       ; slot offset in words
STFP     s16
TAILCALL label

Memory

LOAD32
STORE32
LOAD8U
STORE8
MEMCPY            ; memmove semantics

Arithmetic & bitwise

ADD SUB MUL DIVS NEG
AND OR XOR NOT
SHL SHR

Compare

EQ LT GT LE GE

Convenience

ADDI     s16
SUBI     s16
INC
DEC
MODS
LOAD_OFF  s16      ; base -> *(base+imm) (bytes)
STORE_OFF s16      ; base value -> store32(base+imm)


⸻

8) Example program (full, runnable style)

This example copies a string within linear memory using MEMCPY and halts.

.module "memcpy_demo"

.const SRC = 0x0100
.const DST = 0x0200
.const LEN = 6

.code
main:
  ; copy LEN bytes from SRC to DST
  PUSHI DST
  PUSHI SRC
  PUSHI LEN
  MEMCPY

  HALT

.data
.org SRC
  .ascii "hello", 0   ; 6 bytes including terminator

.end


⸻

9) Assembler output format (ZVM1 single-file container)

The assembler outputs exactly one artifact: a program image file that contains:
    •    The code segment bytes (what IP executes)
    •    The initial linear memory bytes (what MEM starts as)
    •    Metadata needed to load/run the program (sizes, entry point)

9.1 File layout (little-endian)

The output file begins with a fixed-size 28-byte header, followed by the code blob and then the memory-initialization blob:

Offset  Size  Field
0       4     Magic = "ZVM1"
4       2     Version = 0x0001
6       2     Flags = 0 (reserved)
8       4     CodeSize (bytes)
12      4     MemInitSize (bytes)
16      4     MemTotalSize (bytes)   ; VM allocates this much linear memory at runtime (>= MemInitSize)
20      4     EntryIP (bytes)        ; starting instruction pointer (usually 0)
24      4     Reserved = 0

Then:
    •    Code[CodeSize]
    •    MemInit[MemInitSize]

9.2 Loader behavior

A conforming VM loader:
    •    Validates Magic/Version
    •    Allocates MemTotalSize bytes and zero-fills them
    •    Copies MemInitSize bytes from MemInit into mem[0..MemInitSize-1]
    •    Loads the Code blob and sets ip = EntryIP

9.3 Notes

    •    MemTotalSize lets programs reserve additional zeroed memory beyond the initialized data emitted by .data.
    •    MemInitSize is simply the highest byte written by .data emission (including .org moves).
    •    This format intentionally avoids relocations; all addr32 targets are absolute byte offsets in Code.

⸻

10) Implementation notes (so your assembler is easy)

10.1 Two-pass assembly (easiest)
    •    Pass 1: parse, track section offsets, record label addresses, emit placeholder bytes
    •    Pass 2: resolve fixups and patch

10.2 Fixups

You only need one fixup type for v1:
    •    addr32 for JMP/JZ/JNZ/CALL/TAILCALL
And one “address constant” style:
    •    u32 from data label for PUSHI label

10.3 Instruction encoding mapping

Use the opcode numbers from the VM spec and append immediate bytes.

Example encoding rules:
    •    PUSHI x → [0x07] + u32(x)
    •    JMP L   → [0x04] + u32(addr(L))
    •    LDFP -4 → [0x12] + s16(-4)

