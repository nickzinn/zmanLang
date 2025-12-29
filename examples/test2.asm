.module "test2"

; ------------------------------------------------------------
; test2.asm
; A bigger integration test for the assembler + VM.
;
; Exercises:
; - Directives: .module .const .code .data .org .align .byte .word .asciz .zero .end
; - Fixups / expressions: label +/- number, constants
; - ISA: CALL/RET/ENTER/LDFP/STFP, loops/branches, ADD/MUL/DIVS/MODS, LOAD8U/LOAD32, MEMCPY
; - Syscalls: 1 print_u32, 2 print_i32, 3 putchar, 4 write(ptr,len)
; ------------------------------------------------------------

.const ARRAY_N = 5

.code
.entry main
main:
  ; header
  PUSHI str_header
  CALL write_cstr
  POP

  ; factorial(6)
  PUSHI str_fact
  CALL write_cstr
  POP

  PUSHI 6
  CALL fact_u32
  SYSCALL 1

  PUSHI 10
  SYSCALL 3

  ; fib(10) (tail-recursive)
  PUSHI str_fib
  CALL write_cstr
  POP

  PUSHI 10
  CALL fib_u32
  SYSCALL 1

  PUSHI 10
  SYSCALL 3

  ; signed math: (-42 + 100) / 2 = 29
  PUSHI str_math
  CALL write_cstr
  POP

  PUSHI -42
  PUSHI 100
  ADD
  PUSHI 2
  DIVS
  SYSCALL 2

  PUSHI 10
  SYSCALL 3

  ; gcd(48,18)
  PUSHI str_gcd
  CALL write_cstr
  POP

  PUSHI 48
  PUSHI 18
  CALL gcd_u32
  SYSCALL 1

  PUSHI 10
  SYSCALL 3

  ; sum(array_u32, ARRAY_N)
  PUSHI str_sum
  CALL write_cstr
  POP

  PUSHI array_u32
  PUSHI ARRAY_N
  CALL sum_u32_array
  SYSCALL 1

  PUSHI 10
  SYSCALL 3

  ; memcopy demo: copy src string into buffer, then print it
  PUSHI str_copy
  CALL write_cstr
  POP

  PUSHI buf_copy
  PUSHI str_src
  CALL copy_cstr
  POP

  PUSHI buf_copy
  CALL write_cstr
  POP

  PUSHI 10
  SYSCALL 3

  ; store/load demo (STORE32/STORE8) with self-check (no extra output)
  ; If anything is wrong, the VM will TRAP.
  PUSHI buf_copy+32
  PUSHI 0x12345678
  STORE32

  PUSHI buf_copy+32
  LOAD32
  PUSHI 0x12345678
  EQ
  JNZ store_ok32
  TRAP 0x0100
store_ok32:

  PUSHI buf_copy+40
  PUSHI 0xAB
  STORE8

  PUSHI buf_copy+40
  LOAD8U
  PUSHI 0xAB
  EQ
  JNZ store_ok8
  TRAP 0x0101
store_ok8:

  ; byte blob demo (LOAD8U + label+offset constant)
  PUSHI str_blob
  CALL write_cstr
  POP

  PUSHI byte_blob
  LOAD8U
  SYSCALL 1
  PUSHI 32
  SYSCALL 3

  PUSHI byte_blob+1
  LOAD8U
  SYSCALL 1
  PUSHI 32
  SYSCALL 3

  PUSHI byte_blob+2
  LOAD8U
  SYSCALL 1
  PUSHI 32
  SYSCALL 3

  PUSHI byte_blob+3
  LOAD8U
  SYSCALL 1

  PUSHI 10
  SYSCALL 3

  HALT

; ------------------------------------------------------------
; write_cstr(ptr) -> 0
; Prints a NUL-terminated string by computing strlen and using syscall 4.
; Arg layout on entry (after CALL):
;   ptr is at [fp-3]
; ------------------------------------------------------------
write_cstr:
  ENTER 0

  LDFP -3
  DUP
  CALL strlen_u32
  ; stack now: ptr len
  SYSCALL 4

  PUSHI 0
  RET 1

; ------------------------------------------------------------
; strlen_u32(ptr) -> len
; Simple C-string length (bytes before NUL).
; Uses LOAD8U and mutates the ptr argument in place.
; ------------------------------------------------------------
strlen_u32:
  ENTER 1          ; local0 = len

  PUSHI 0
  STFP 0

strlen_loop:
  LDFP -3
  LOAD8U
  DUP
  JZ strlen_done
  POP

  LDFP 0
  INC
  STFP 0

  LDFP -3
  ADDI 1
  STFP -3

  JMP strlen_loop

strlen_done:
  POP
  LDFP 0
  RET 1

; ------------------------------------------------------------
; fact_u32(n) -> n!
; Iterative factorial. Demonstrates locals + STFP/LDFP.
; Mutates the argument slot (n) in place.
; ------------------------------------------------------------
fact_u32:
  ENTER 1          ; local0 = result

  PUSHI 1
  STFP 0

fact_loop:
  LDFP -3
  DUP
    JZ fact_done

  ; result *= n
  LDFP 0
  OVER            ; n result n
  SWAP            ; n n result
  MUL             ; n (n*result)
  SWAP            ; (n*result) n
  POP
  STFP 0

  ; n--
  LDFP -3
  DEC
  STFP -3

    JMP fact_loop

fact_done:
  POP
  LDFP 0
  RET 1

; ------------------------------------------------------------
; fib_u32(n) -> fib(n)
; Computes Fibonacci using a tail-recursive helper fib2(n,a,b).
; ------------------------------------------------------------
fib_u32:
  ENTER 0

  ; fib2(n, 0, 1)
  LDFP -3
  PUSHI 0
  PUSHI 1
  CALL fib2
  RET 1

; ------------------------------------------------------------
; fib2(n,a,b) -> a when n==0 else fib2(n-1, b, a+b)
; Arg layout (argc=3): n=[fp-5], a=[fp-4], b=[fp-3]
; Uses TAILCALL to keep call depth constant.
; ------------------------------------------------------------
fib2:
  ENTER 0

fib2_loop:
  ; if (n == 0) return a
  LDFP -5
  DUP
  JZ fib2_done
  POP

  ; sum = a + b
  LDFP -4
  LDFP -3
  ADD
  ; stack: sum

  ; a = b
  LDFP -3
  STFP -4

  ; b = sum
  STFP -3

  ; n = n - 1
  LDFP -5
  DEC
  STFP -5

  ; TAILCALL doesn't move fp/sp; jump to the loop label to avoid re-running ENTER.
  LEAVE
  TAILCALL fib2_loop

fib2_done:
  POP
  LDFP -4
  RET 3

; ------------------------------------------------------------
; gcd_u32(a,b) -> gcd
; Euclid's algorithm. Uses MODS.
; Arg layout: a=[fp-4], b=[fp-3]
; ------------------------------------------------------------
gcd_u32:
  ENTER 0

gcd_loop:
  ; if (b == 0) return a
  LDFP -3
  DUP
  JZ gcd_done
  POP

  ; t = a % b
  LDFP -4
  LDFP -3
  MODS
  ; stack: t

  ; a = b
  LDFP -3
  STFP -4

  ; b = t
  STFP -3

  JMP gcd_loop

gcd_done:
  POP
  LDFP -4
  RET 2

; ------------------------------------------------------------
; sum_u32_array(ptr,count) -> sum
; Sums count u32 values from linear memory.
; Arg layout: ptr=[fp-4], count=[fp-3]
; ------------------------------------------------------------
sum_u32_array:
  ENTER 2          ; local0=sum, local1=i

  PUSHI 0
  STFP 0
  PUSHI 0
  STFP 1

sum_loop:
  LDFP 1
  LDFP -3
  LT
    JZ sum_done

  ; addr = ptr + i*4
  LDFP -4
  LDFP 1
  PUSHI 4
  MUL
  ADD
  LOAD32

  LDFP 0
  ADD
  STFP 0

  LDFP 1
  INC
  STFP 1

    JMP sum_loop

sum_done:
  LDFP 0
  RET 2

; ------------------------------------------------------------
; copy_cstr(dest, src) -> dest
; Copies src (including NUL) into dest using MEMCPY.
; Arg layout: dest=[fp-4], src=[fp-3]
; ------------------------------------------------------------
copy_cstr:
  ENTER 0

  ; compute len = strlen(src) + 1
  LDFP -3
  DUP
  CALL strlen_u32
  PUSHI 1
  ADD
  ; stack: src len

  LDFP -4
  ; stack: src len dest
  SWAP
  ROT
  SWAP
  ; stack: dest src len
  MEMCPY

  LDFP -4
  RET 2

.data
.org 0x0100
.align 16

str_header:
  .asciz "== test2: asm/vm features ==\n"

str_fact:
  .asciz "factorial(6) = "

str_fib:
  .asciz "fib(10) = "

str_math:
  .asciz "(-42 + 100) / 2 = "

str_gcd:
  .asciz "gcd(48,18) = "

str_sum:
  .asciz "sum(array) = "

str_copy:
  .asciz "copied string: "

str_blob:
  .asciz "byte_blob bytes (dec): "

str_src:
  .asciz "Hello from MEMCPY!"

.align 4
array_u32:
  .word 1, 2, 3, 4, 5

byte_blob:
  .byte 0xDE, 0xAD, 0xBE, 0xEF

.align 16
buf_copy:
  .zero 64

.end
