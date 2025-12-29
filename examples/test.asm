.module "hello"

.const MSG = 0x0100
.const LEN = 12

.code
.entry main

print:
  ENTER 0

  PUSHI MSG
  PUSHI LEN
  SYSCALL 4
  
  LDFP -3           ;load loop couter for printing
  SYSCALL 1
  
  PUSHI 10
  SYSCALL 3         ; newline

  PUSHI 1
  RET 0


main:
  PUSHI 10          ; counter = 10
  
loop:
  DUP               
  JZ done           ; if counter == 0, exit loop
  CALL print        ; call print function
  POP               ; discard return value
  DEC
  JMP loop          ; jump back to loop start
done:
  POP               ; pop the counter
  HALT


.data
.org 0x0100
  .ascii "Count Down: "
  
.end
