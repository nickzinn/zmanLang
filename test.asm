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

__zman_strcat:
  ENTER 5
  LDFP -4
  DUP
  LOAD32
  STFP 0
  POP
  LDFP -3
  DUP
  LOAD32
  STFP 1
  POP
  LDFP 0
  LDFP 1
  ADD
  STFP 2
  LDFP 2
  ADDI 4
  SYSCALL 6
  DUP
  STFP 3
  DUP
  LDFP 2
  STORE32
  DUP
  ADDI 4
  STFP 4
  POP
  LDFP 4
  LDFP -4
  ADDI 4
  LDFP 0
  MEMCPY
  LDFP 4
  LDFP 0
  ADD
  LDFP -3
  ADDI 4
  LDFP 1
  MEMCPY
  LDFP 3
  RET 2

main:
  PUSHI g_a
  PUSHI 40
  STORE32
  PUSHI g_b
  PUSHI 2
  STORE32
  PUSHI g_a
  LOAD32
  PUSHI g_b
  LOAD32
  ADD
  SYSCALL 8
  CALL __zman_print
  POP
  PUSHI str_lit_0
  CALL __zman_print
  POP
  PUSHI 7
  PUSHI 3
  PUSHI 1
  ADD
  MUL
  SYSCALL 8
  CALL __zman_print
  POP
  PUSHI str_lit_1
  CALL __zman_print
  POP
  PUSHI 7
  PUSHI 2
  DIVS
  SYSCALL 8
  CALL __zman_print
  POP
  PUSHI str_lit_2
  CALL __zman_print
  POP
  PUSHI 7
  PUSHI 2
  MODS
  SYSCALL 8
  CALL __zman_print
  POP
  PUSHI str_lit_3
  CALL __zman_print
  POP
  PUSHI 5
  NEG
  PUSHI 2
  ADD
  SYSCALL 8
  CALL __zman_print
  POP
  PUSHI str_lit_4
  CALL __zman_print
  POP
  PUSHI str_lit_5
  SYSCALL 9
  PUSHI 1
  ADD
  SYSCALL 8
  CALL __zman_print
  POP
  PUSHI str_lit_6
  CALL __zman_print
  POP
  HALT

.data
  .align 4

g_a:
  .word 0
g_b:
  .word 0

str_lit_0:
  .word 1
  .ascii "\n"
  .align 4

str_lit_1:
  .word 1
  .ascii "\n"
  .align 4

str_lit_2:
  .word 1
  .ascii "\n"
  .align 4

str_lit_3:
  .word 1
  .ascii "\n"
  .align 4

str_lit_4:
  .word 1
  .ascii "\n"
  .align 4

str_lit_5:
  .word 3
  .ascii "123"
  .align 4

str_lit_6:
  .word 1
  .ascii "\n"
  .align 4

.end
