.module "bench"

; Simple VM benchmark: tight loop with no I/O.
; Measures dispatch + stack ops + some ALU + some memory activity.
;
; Tune ITER via examples/bench.sh (it rewrites this constant into a temp file).
.const ITER = 20000000

; Memory regions used (must stay within the VM's linear memory, which is >= 64KiB)
.const WORD_BASE = 0x4000
.const BYTE_BASE = 0x3000
.const MASK_1023 = 1023

.code
.entry main

; helper(acc, counter) -> acc'
; Exercises: CALL/RET/ENTER/LDFP/STFP plus a few ALU ops.
helper:
  ENTER 1

  ; local0 = acc + counter
  LDFP -4
  LDFP -3
  ADD
  STFP 0

  ; return local0 ^ (counter << 1)
  LDFP 0
  LDFP -3
  PUSHI 1
  SHL
  XOR
  RET 2

main:
  ; stack: [counter acc]
  PUSHI ITER
  PUSHI 0

loop:
  ; if counter == 0 -> done
  OVER
  JZ done

  ; Periodically call helper(acc, counter) every 16 iterations.
  ; This adds some call/frame overhead into the benchmark without I/O.
  OVER
  PUSHI 15
  AND
  JNZ skip_helper

  ; Build args from [counter acc] -> push acc, counter
  DUP2
  SWAP
  CALL helper
  ; stack now: [counter acc rv] -> replace acc with rv
  SWAP
  POP

skip_helper:

  ; ------------------------------------------------------------
  ; Mix in a few ALU ops (cheap, exercises more opcodes)
  ; acc ^= counter
  OVER
  XOR

  ; ------------------------------------------------------------
  ; 32-bit memory store+load at WORD_BASE + ((counter & 1023) << 2)
  ; After this block: stack remains [counter acc]
  OVER
  PUSHI MASK_1023
  AND
  PUSHI 2
  SHL
  PUSHI WORD_BASE
  ADD
  ; stack: [counter acc addr]
  SWAP
  ; stack: [counter addr acc]
  DUP2
  ; stack: [counter addr acc addr acc]
  STORE32
  ; stack: [counter addr acc]
  SWAP
  ; stack: [counter acc addr]
  LOAD32
  ; stack: [counter acc loaded]
  ADD
  ; stack: [counter acc]

  ; ------------------------------------------------------------
  ; Byte store+load at BYTE_BASE + (counter & 1023)
  ; Use DUP2 to preserve acc across STORE8.
  DUP2
  ; stack: [counter acc counter acc]
  SWAP
  ; stack: [counter acc acc counter]
  PUSHI MASK_1023
  AND
  PUSHI BYTE_BASE
  ADD
  ; stack: [counter acc acc baddr]
  SWAP
  ; stack: [counter acc baddr acc]
  STORE8
  ; stack: [counter acc]

  ; load back and fold into acc (acc += byte)
  OVER
  PUSHI MASK_1023
  AND
  PUSHI BYTE_BASE
  ADD
  LOAD8U
  ADD
  ; stack: [counter acc]

  ; ------------------------------------------------------------
  ; counter-- (mutate the counter slot)
  SWAP
  DEC
  SWAP
  JMP loop

done:
  POP   ; pop acc
  POP   ; pop counter (0)
  HALT

.end
