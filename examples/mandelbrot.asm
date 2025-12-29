.module "mandelbrot"

; Mandelbrot checksum benchmark (integer scaled).
;
; Notes:
; - Uses a small SCALE so 32-bit MUL doesn't overflow for typical values.
; - No I/O in the hot loops; prints a final checksum (sum of per-pixel iters).
;
; Tunables (can be rewritten by examples/mandelbrot.sh):
.const WIDTH   = 80
.const HEIGHT  = 60
.const MAXITER = 64

; Fixed scaling factor (not a power-of-two requirement; we use DIVS).
.const SCALE   = 1024

; Plane: x in [-2.0, 1.0], y in [-1.5, 1.5]
; Scaled by SCALE.
.const X_START = -2048      ; -2.0 * SCALE
.const Y_START = -1536      ; -1.5 * SCALE

; We compute X_STEP and Y_STEP at runtime to keep the benchmark tunable.
; STEP = (3.0 * SCALE) / DIM
.const RANGE_SCALED = 3072  ; 3.0 * SCALE

; Escape radius squared: 4.0 * SCALE
.const THRESH  = 4096

.code
.entry main

main:
  ; Locals (ENTER 11):
  ; 0 sum
  ; 1 y0
  ; 2 y_i
  ; 3 x0
  ; 4 x_i
  ; 5 zx
  ; 6 zy
  ; 7 iter
  ; 8 zx2
  ; 9 zy2
  ; 10 zxy
  ; 11 x_step
  ; 12 y_step
  ENTER 13

  PUSHI 0
  STFP 0

  ; x_step = RANGE_SCALED / WIDTH
  PUSHI RANGE_SCALED
  PUSHI WIDTH
  DIVS
  STFP 11

  ; y_step = RANGE_SCALED / HEIGHT
  PUSHI RANGE_SCALED
  PUSHI HEIGHT
  DIVS
  STFP 12

  PUSHI Y_START
  STFP 1

  PUSHI HEIGHT
  STFP 2

row_loop:
  LDFP 2
  JZ end

  PUSHI X_START
  STFP 3

  PUSHI WIDTH
  STFP 4

col_loop:
  LDFP 4
  JZ next_row

  PUSHI 0
  STFP 5
  PUSHI 0
  STFP 6
  PUSHI 0
  STFP 7

iter_loop:
  ; if iter == MAXITER -> done_pixel
  LDFP 7
  PUSHI MAXITER
  EQ
  JNZ done_pixel

  ; zx2 = (zx*zx)/SCALE
  LDFP 5
  DUP
  MUL
  PUSHI SCALE
  DIVS
  STFP 8

  ; zy2 = (zy*zy)/SCALE
  LDFP 6
  DUP
  MUL
  PUSHI SCALE
  DIVS
  STFP 9

  ; if zx2 + zy2 > THRESH -> done_pixel
  LDFP 8
  LDFP 9
  ADD
  PUSHI THRESH
  GT
  JNZ done_pixel

  ; zxy = (zx*zy)/SCALE
  LDFP 5
  LDFP 6
  MUL
  PUSHI SCALE
  DIVS
  STFP 10

  ; zy = 2*zxy + y0
  LDFP 10
  PUSHI 1
  SHL
  LDFP 1
  ADD
  STFP 6

  ; zx = (zx2 - zy2) + x0
  LDFP 8
  LDFP 9
  SUB
  LDFP 3
  ADD
  STFP 5

  ; iter++
  LDFP 7
  INC
  STFP 7

  JMP iter_loop

done_pixel:
  ; sum += iter
  LDFP 0
  LDFP 7
  ADD
  STFP 0

  ; x0 += X_STEP
  LDFP 3
  LDFP 11
  ADD
  STFP 3

  ; x_i--
  LDFP 4
  DEC
  STFP 4

  JMP col_loop

next_row:
  ; y0 += Y_STEP
  LDFP 1
  LDFP 12
  ADD
  STFP 1

  ; y_i--
  LDFP 2
  DEC
  STFP 2

  JMP row_loop

end:
  ; Print checksum
  LDFP 0
  SYSCALL 1
  PUSHI 10
  SYSCALL 3
  HALT

.end
