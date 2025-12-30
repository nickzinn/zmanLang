StackVM-32 Spec (v1)

1. Overview

StackVM-32 is a stack-based virtual machine with:
    •    An operand/value stack of 32-bit words
    •    A frame pointer (FP) for function call frames
    •    An instruction pointer (IP) into a bytecode array
    •    A linear memory of bytes addressed by 32-bit offsets

This VM is designed for simple interpretation and straightforward assembly/disassembly.

⸻

2. Data Types & Conventions

2.1 Word type
    •    Word: unsigned 32-bit value (u32)
    •    Many operations interpret operands as signed 32-bit (s32) where specified (comparisons, DIVS, MODS, shifts per below).

2.2 Endianness
    •    All multi-byte immediates in bytecode are little-endian
    •    Memory LOAD32/STORE32 uses little-endian encoding

2.3 Booleans
    •    0 = false
    •    1 = true
    •    Comparisons must push exactly 0 or 1

2.4 Arithmetic overflow
    •    ADD/SUB/MUL/NEG/ADDI/SUBI/INC/DEC wrap mod 2^32 (two’s complement behavior).
    •    Bitwise ops are on u32.

2.5 Shifts
    •    Shift amount is masked: shift &= 31
    •    SHL: logical left shift
    •    SHR: logical right shift (zero-fill)

⸻

3. Machine State

The VM state consists of:
    •    u8* code and u32 code_size
    •    u8* mem and u32 mem_size
    •    u32 ip — instruction pointer (byte index into code)
    •    u32 sp — stack pointer (index into stack[], next free slot)
    •    u32 fp — frame pointer (index into stack[], base of current frame)
    •    u32 stack[] — value stack storage (size chosen by implementation)

3.1 Stack indexing
    •    stack[0..sp-1] are live values
    •    push(x) writes stack[sp++] = x
    •    pop() returns stack[--sp]

3.2 Error model

The VM may stop with:
    •    HALT (normal termination)
    •    TRAP instruction
    •    Runtime trap (e.g., out-of-bounds memory, divide by zero, stack underflow/overflow, invalid IP)

Implementations may represent traps however they like, but must stop executing the current program.

⸻

4. Bytecode Encoding

All instructions are variable-length:
    •    1 byte opcode
    •    followed by 0 / 1 / 2 / 4 bytes of immediate operand data

4.1 Immediate formats (all little-endian)
    •    u8: 1 byte
    •    u16: 2 bytes
    •    s16: 2 bytes (two’s complement)
    •    u32: 4 bytes
    •    addr32: 4 bytes absolute code address (u32 byte offset)

4.2 IP advance
    •    ip advances by instruction length unless control-flow changes it.

4.3 Addressing
    •    addr32 targets an absolute byte offset into code.
    •    It is a trap if addr32 >= code_size.

4.4 Program Image Format (ZVM1)

Programs are distributed as a single file containing a small header plus the code and initial memory image.

File layout (little-endian):

Offset  Size  Field
0       4     Magic = "ZVM1"
4       2     Version = 0x0001
6       2     Flags = 0 (reserved)
8       4     CodeSize (bytes)
12      4     MemInitSize (bytes)
16      4     MemTotalSize (bytes)
20      4     EntryIP (bytes)
24      4     Reserved = 0

Followed by:
    •    Code[CodeSize]
    •    MemInit[MemInitSize]

Loader requirements:
    •    Validate Magic/Version
    •    Allocate MemTotalSize bytes for linear memory and zero-fill
    •    Copy MemInit bytes into the start of memory
    •    Set ip = EntryIP and begin execution

Constraints:
    •    MemTotalSize >= MemInitSize
    •    EntryIP < CodeSize (otherwise trap)

⸻

5. Memory Model

5.1 Linear memory
    •    A single byte array mem[0..mem_size-1]

5.2 Bounds checks

All loads/stores must bounds-check:
    •    LOAD8U: requires addr < mem_size
    •    STORE8: requires addr < mem_size
    •    LOAD32: requires addr+4 <= mem_size
    •    STORE32: requires addr+4 <= mem_size
    •    MEMCPY: requires both ranges in-bounds:
dest+len <= mem_size and src+len <= mem_size

Out-of-bounds access is a trap.

5.3 Alignment

No alignment requirement. LOAD32/STORE32 may access any byte address.

5.4 MEMCPY semantics

MEMCPY behaves like memmove:
    •    Overlap is allowed
    •    Copy is as-if into a temporary buffer (or direction-aware copy)

5.5 Optional heap allocation (recommended host ABI)

The VM provides linear memory only; higher-level runtimes (like ZManLang) may
use a simple heap allocator within that linear memory.

Recommended convention (bump allocator):
    •    The heap starts at the first 4-byte aligned address at or above MemInitSize.
    •    Allocations are rounded up to 4-byte alignment.
    •    No free / no garbage collection in v1.

Recommended syscall IDs:
    •    0: exit(code)             stack: code -> (halts)
    •    1: print_u32(x)           stack: x ->
    •    2: print_i32(x)           stack: x ->
    •    3: putchar(ch)            stack: ch ->
    •    4: write(ptr,len)         stack: ptr len ->   (writes bytes from linear memory to stdout)
    •    5: read(ptr,len)          stack: ptr len -> n (reads into linear memory from stdin, returns n)
    •    6: heap_alloc(nbytes)     stack: nbytes -> ptr (4-byte aligned bump alloc; trap on OOM)
    •    7: heap_ptr()             stack: -> ptr       (current bump pointer)
    •    8: text_i32(x)            stack: x -> p        (allocates a ZManLang string object for the decimal text of i32)
    •    9: number(p)              stack: p -> x        (parses a ZManLang string object as i32, traps on invalid/overflow)

⸻

6. Calling Convention & Frames

The VM uses a single value stack for operands and call frames.

6.1 CALL frame layout

When a function is called, the stack contains arguments pushed by the caller, then CALL appends frame metadata:

Before CALL (caller just pushed args):

... arg1 arg2 ... argN

CALL pushes:
    •    return IP (address of next instruction after CALL)
    •    old FP

Then sets FP = SP (after pushing metadata).

After CALL:

... arg1 arg2 ... argN  ret_ip  old_fp
                          ^      ^
                        (FP-2) (FP-1) if you view FP as index to next free slot; see below

Implementation detail: In this spec, fp is defined as the stack index immediately after pushing old_fp (i.e., fp = sp). That makes the saved slots accessible at negative offsets:

    •    saved old FP at stack[fp-1]
    •    saved return IP at stack[fp-2]
    •    last argument at stack[fp-3]
    •    first argument at stack[fp-(2+argc)]

6.2 ENTER / LEAVE
    •    ENTER nlocals reserves nlocals word slots for locals by advancing sp += nlocals.
    •    LEAVE discards locals by setting sp = fp.

6.3 LDFP / STFP offsets

LDFP s16 and STFP s16 address word slots relative to fp:
    •    Effective index: idx = (s32)fp + (s32)offset
    •    Must satisfy 0 <= idx < sp for loads and stores (no “sparse” stores)

Recommended conventions:
    •    Locals at nonnegative offsets: 0, 1, 2, ...
    •    Metadata/args at negative offsets: -1 old_fp, -2 ret_ip, -3 last arg, …

Example (argc=2):
    •    arg1 (first) at fp - (2+2) = fp-4
    •    arg2 at fp-3
    •    ret_ip at fp-2
    •    old_fp at fp-1
    •    local0 at fp+0

6.4 RET argc

RET argc:
    1.    Pop return value rv
    2.    sp = fp (discard locals)
    3.    Restore old_fp = pop()
    4.    Restore ret_ip = pop()
    5.    sp -= argc (discard arguments)
    6.    Push rv
    7.    fp = old_fp; ip = ret_ip

Traps:
    •    stack underflow at any step
    •    invalid restored ip

6.5 TAILCALL addr32

TAILCALL reuses the current frame instead of creating a new one. Spec’d as:

TAILCALL is a control-transfer intended for tail calls that preserves the current call frame metadata.

TAILCALL addr32:
    •    Effect: set ip = addr32.
    •    Does not push a new return IP / FP.
    •    Does not change fp.
    •    Does not adjust sp.

This means a tail-called function returns directly to the original caller (because the saved ret_ip/old_fp for the current frame remain in place).

Argument convention (Option A):
    •    Because fp is unchanged, the callee must read its arguments from the current frame’s argument slots (negative offsets from fp, e.g. fp-3, fp-4, ...).
    •    Caller/compiler must place the callee’s arguments into those slots explicitly, typically by using STFP with negative offsets.
    •    To avoid unbounded stack growth, code should typically execute LEAVE before TAILCALL to discard the current function’s locals (sp = fp).

Notes:
    •    TAILCALL does not encode an argument count; the calling code must follow a fixed, agreed-upon layout for the callee.
    •    If the caller needs temporary values to compute arguments, it can use locals before LEAVE, then write final arguments into the frame slots, then LEAVE, then TAILCALL.

⸻

7. Instruction Set (50 opcodes)

7.1 Stack effect notation
    •    a b -> c means pop b, pop a, push c
    •    Top of stack is rightmost element in examples

7.2 Opcode Map

Opcodes are single-byte values. This table assigns stable numeric IDs.

Control (4)
    •    0x00 NOP
    •    0x01 HALT
    •    0x02 SYSCALL u8
    •    0x03 TRAP u16

Branch (3)
    •    0x04 JMP addr32
    •    0x05 JZ addr32
    •    0x06 JNZ addr32

Stack ops (7)
    •    0x07 PUSHI u32
    •    0x08 POP
    •    0x09 DUP
    •    0x0A DUP2
    •    0x0B SWAP
    •    0x0C ROT
    •    0x0D OVER

Calls & frames (6)
    •    0x0E CALL addr32
    •    0x0F RET u8
    •    0x10 ENTER u16
    •    0x11 LEAVE
    •    0x12 LDFP s16
    •    0x13 STFP s16

Memory (5)
    •    0x14 LOAD32
    •    0x15 STORE32
    •    0x16 LOAD8U
    •    0x17 STORE8
    •    0x18 MEMCPY (memmove semantics)

Arithmetic & bitwise (10)
    •    0x19 ADD
    •    0x1A SUB
    •    0x1B MUL
    •    0x1C DIVS
    •    0x1D NEG
    •    0x1E AND
    •    0x1F OR
    •    0x20 XOR
    •    0x21 SHL
    •    0x22 SHR

Compare (5)
    •    0x23 EQ
    •    0x24 LT (signed)
    •    0x25 GT (signed)
    •    0x26 LE (signed)
    •    0x27 GE (signed)

Convenience (10)
    •    0x28 ADDI s16
    •    0x29 SUBI s16
    •    0x2A INC
    •    0x2B DEC
    •    0x2C MODS
    •    0x2D NOT
    •    0x2E CALLI
    •    0x2F TAILCALL addr32
    •    0x30 LOAD_OFF s16
    •    0x31 STORE_OFF s16

(50 total: 0x00–0x31 inclusive)

⸻

8. Per-instruction Semantics

8.1 Control

NOP (0x00)
    •    Size: 1
    •    Stack: no change
    •    Effect: none

HALT (0x01)
    •    Size: 1
    •    Effect: stop execution normally

SYSCALL u8 (0x02)
    •    Size: 2
    •    Stack: VM-defined (host ABI)
    •    Effect: invoke host function id
    •    Trap: host may signal trap
    •    Notes: see §5.5 for the recommended syscall IDs used by this project.

TRAP u16 (0x03)
    •    Size: 3
    •    Effect: stop with trap code

⸻

8.2 Branch

JMP addr32 (0x04)
    •    Size: 5
    •    Effect: ip = addr32
    •    Trap: addr32 >= code_size

JZ addr32 (0x05)
    •    Size: 5
    •    Stack: cond ->
    •    Effect: if cond == 0 then ip = addr32 else continue
    •    Trap: stack underflow; invalid addr32 when taken

JNZ addr32 (0x06)
    •    Size: 5
    •    Stack: cond ->
    •    Effect: if cond != 0 then ip = addr32 else continue
    •    Trap: stack underflow; invalid addr32 when taken

⸻

8.3 Stack ops

PUSHI u32 (0x07)
    •    Size: 5
    •    Stack: -> imm
    •    Trap: stack overflow

POP (0x08)
    •    Size: 1
    •    Stack: x ->
    •    Trap: underflow

DUP (0x09)
    •    Size: 1
    •    Stack: x -> x x
    •    Trap: underflow/overflow

DUP2 (0x0A)
    •    Size: 1
    •    Stack: a b -> a b a b
    •    Trap: underflow/overflow

SWAP (0x0B)
    •    Size: 1
    •    Stack: a b -> b a
    •    Trap: underflow

ROT (0x0C)
    •    Size: 1
    •    Stack: a b c -> b c a
    •    Trap: underflow

OVER (0x0D)
    •    Size: 1
    •    Stack: a b -> a b a
    •    Trap: underflow/overflow

⸻

8.4 Calls & frames

CALL addr32 (0x0E)
    •    Size: 5
    •    Stack: ... args -> ... args ret_ip old_fp
    •    Effect:
    1.    push ret_ip = ip_after_call
    2.    push old_fp = fp
    3.    set fp = sp
    4.    set ip = addr32
    •    Trap: stack overflow; invalid addr32

CALLI (0x2E)
    •    Size: 1
    •    Stack: addr -> (consumes addr)
    •    Effect: like CALL addr, but addr popped from stack
    •    Trap: underflow; invalid addr; stack overflow

ENTER u16 (0x10)
    •    Size: 3
    •    Stack: no change
    •    Effect: sp += nlocals
    •    Locals initial value: implementation-defined (recommend zeroing for deterministic behavior)
    •    Trap: stack overflow

LEAVE (0x11)
    •    Size: 1
    •    Effect: sp = fp
    •    Trap: (optional) if fp > sp or corrupted frame, treat as trap

LDFP s16 (0x12)
    •    Size: 3
    •    Stack: -> value
    •    Effect: push stack[fp + offset]
    •    Trap: index out of range; overflow

STFP s16 (0x13)
    •    Size: 3
    •    Stack: value ->
    •    Effect: stack[fp + offset] = value
    •    Trap: underflow; index out of range (must satisfy 0 <= fp+offset < sp)

RET u8 (0x0F)
    •    Size: 2
    •    Stack: rv -> ... rv (replaces args+frame with rv)
    •    Effect: as defined in §6.4
    •    Trap: underflow; invalid restored ip

TAILCALL addr32 (0x2F)
    •    Size: 5
    •    Effect: ip = addr32 without pushing return ip/fp; fp and sp are unchanged
    •    Trap: invalid addr32
    •    Notes (Option A): caller/compiler is responsible for discarding locals (typically via LEAVE) and placing callee arguments into the current frame’s argument slots (negative offsets from fp) before executing TAILCALL.

⸻

8.5 Memory

LOAD32 (0x14)
    •    Size: 1
    •    Stack: addr -> value
    •    Effect: read mem[addr..addr+3] little-endian
    •    Trap: underflow; OOB

STORE32 (0x15)
    •    Size: 1
    •    Stack: addr value ->
    •    Effect: write 4 bytes little-endian
    •    Trap: underflow; OOB

LOAD8U (0x16)
    •    Size: 1
    •    Stack: addr -> value
    •    Effect: push zero-extended mem[addr]
    •    Trap: underflow; OOB

STORE8 (0x17)
    •    Size: 1
    •    Stack: addr value ->
    •    Effect: mem[addr] = (value & 0xFF)
    •    Trap: underflow; OOB

MEMCPY (0x18) (memmove semantics)
    •    Size: 1
    •    Stack: dest src len ->
    •    Effect: move len bytes from src to dest, overlap-safe
    •    Trap: underflow; OOB (either range)

⸻

8.6 Arithmetic & bitwise

All are size 1 and trap on underflow. Results are u32 wrapping.

ADD (0x19): a b -> a+b
SUB (0x1A): a b -> a-b
MUL (0x1B): a b -> a*b
NEG (0x1D): a -> -a

DIVS (0x1C) (signed): a b -> (s32)a / (s32)b
    •    Trap: divide by zero
    •    (Optional) also trap on INT_MIN / -1 overflow; if not trapped, wrap implementation-defined—recommended to trap for determinism.

MODS (0x2C) (signed): a b -> (s32)a % (s32)b
    •    Trap: divide by zero

AND (0x1E): a b -> a&b
OR (0x1F): a b -> a|b
XOR (0x20): a b -> a^b
NOT (0x2D): a -> ~a

SHL (0x21): value shift -> value << (shift&31)
SHR (0x22): value shift -> value >> (shift&31) (logical)

⸻

8.7 Comparisons (signed)

All are size 1; trap on underflow; push 0 or 1.

Let a_s = (s32)a, b_s = (s32)b.

EQ (0x23): a b -> (a==b)
LT (0x24): a b -> (a_s < b_s)
GT (0x25): a b -> (a_s > b_s)
LE (0x26): a b -> (a_s <= b_s)
GE (0x27): a b -> (a_s >= b_s)

⸻

8.8 Convenience immediates

ADDI s16 (0x28)
    •    Size: 3
    •    Stack: x -> x + imm
    •    Trap: underflow

SUBI s16 (0x29)
    •    Size: 3
    •    Stack: x -> x - imm
    •    Trap: underflow

INC (0x2A)
    •    Size: 1
    •    Stack: x -> x+1
    •    Trap: underflow

DEC (0x2B)
    •    Size: 1
    •    Stack: x -> x-1
    •    Trap: underflow

LOAD_OFF s16 (0x30)
    •    Size: 3
    •    Stack: base -> value
    •    Effect: addr = base + imm (byte address), then LOAD32 at addr
    •    Trap: underflow; OOB

STORE_OFF s16 (0x31)
    •    Size: 3
    •    Stack: base value ->
    •    Effect: addr = base + imm, then STORE32 at addr
    •    Trap: underflow; OOB

⸻

9. Validation Recommendations (optional but helpful)

An implementation may additionally validate before execution:
    •    ip always points to a valid opcode boundary (or trap if decoding fails)
    •    all addr32 targets are within code_size
    •    stack capacity configured

⸻

10. Example: CALL frame offsets (argc=2)

Caller:

PUSHI a1
PUSHI a2
CALL f

At function entry (before ENTER):
    •    old_fp at fp-1
    •    ret_ip at fp-2
    •    arg2 at fp-3
    •    arg1 at fp-4

Locals after ENTER 2:
    •    local0 at fp+0
    •    local1 at fp+1
