.module "zman_program"

.code
.entry main

__zman_print:
  ENTER 0
  LDFP -3
  DUP
  LOAD32
  SWAP
  ADDI 4
  SWAP
  SYSCALL 4
  PUSHI 0
  RET 1

main:
  PUSHI str_lit_0
  CALL __zman_print
  POP
  HALT

.data
  .align 4

str_lit_0:
  .word 14
  .ascii "Hello, world!\n"
  .align 4

.end
