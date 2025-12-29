.module "test3_heap"

; ------------------------------------------------------------
; test3.asm
; Minimal heap (bump allocator) integration test.
;
; Exercises:
; - New syscalls:
;     6 heap_alloc(nbytes) -> ptr
; - STORE32/LOAD32 using heap pointers
; ------------------------------------------------------------

.const MSG     = 0x0100
.const MSG_LEN = 18

.code
.entry main

main:
  ; print banner
  PUSHI MSG
  PUSHI MSG_LEN
  SYSCALL 4

  ; allocate 16 bytes for an "array" object:
  ;   [0..3]   u32 len (=3)
  ;   [4..7]   u32 elem0 (=7)
  ;   [8..11]  u32 elem1 (=8)
  ;   [12..15] u32 elem2 (=9)
  PUSHI 16
  SYSCALL 6        ; -> ptr

  ; store len
  DUP
  PUSHI 3
  STORE32

  ; store elem0 at ptr+4
  DUP
  PUSHI 4
  ADD
  PUSHI 7
  STORE32

  ; store elem1 at ptr+8
  DUP
  PUSHI 8
  ADD
  PUSHI 8
  STORE32

  ; store elem2 at ptr+12
  DUP
  PUSHI 12
  ADD
  PUSHI 9
  STORE32

  ; print elem0 elem1 elem2 as: "7 8 9\n"
  DUP
  PUSHI 4
  ADD
  LOAD32
  SYSCALL 1
  PUSHI 32
  SYSCALL 3

  DUP
  PUSHI 8
  ADD
  LOAD32
  SYSCALL 1
  PUSHI 32
  SYSCALL 3

  DUP
  PUSHI 12
  ADD
  LOAD32
  SYSCALL 1
  PUSHI 10
  SYSCALL 3

  ; discard ptr
  POP

  HALT

.data
.org 0x0100
  .ascii "== test3: heap ==\n"

.end
