#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ASM="$ROOT/bin/svm_asm"
VM="$ROOT/bin/svm_vm"
SRC="$ROOT/examples/mandelbrot.asm"
OUT="$ROOT/build/mandelbrot.zvm"
TMP="$ROOT/build/mandelbrot.tmp.asm"

WIDTH="${1:-640}"
HEIGHT="${2:-480}"
MAXITER="${3:-512}"
REPS="${4:-5}"

if [[ ! -x "$ASM" || ! -x "$VM" ]]; then
  echo "error: build tools not found; run 'make' first" >&2
  exit 2
fi

mkdir -p "$ROOT/build"

# Rewrite tunables into a temp asm and assemble once.
sed -E \
  -e "s/^\\.const[[:space:]]+WIDTH[[:space:]]*=[[:space:]]*.*/.const WIDTH   = ${WIDTH}/" \
  -e "s/^\\.const[[:space:]]+HEIGHT[[:space:]]*=[[:space:]]*.*/.const HEIGHT  = ${HEIGHT}/" \
  -e "s/^\\.const[[:space:]]+MAXITER[[:space:]]*=[[:space:]]*.*/.const MAXITER = ${MAXITER}/" \
  "$SRC" > "$TMP"

"$ASM" "$TMP" "$OUT" >/dev/null

# Warm-up
"$VM" "$OUT" >/dev/null

echo "mandelbrot_bench: WIDTH=$WIDTH HEIGHT=$HEIGHT MAXITER=$MAXITER REPS=$REPS" >&2

times=()

for ((i=1; i<=REPS; i++)); do
  echo "run $i:" >&2
  t_out=$({ /usr/bin/time -p "$VM" "$OUT" >/dev/null; } 2>&1)
  echo "$t_out" >&2
  real_s=$(awk '/^real[[:space:]]+/ { print $2; exit }' <<<"$t_out")
  if [[ -z "${real_s:-}" ]]; then
    echo "error: failed to parse timing output" >&2
    exit 3
  fi
  times+=("$real_s")
done

# Summary stats
if (( ${#times[@]} > 0 )); then
  sorted=( $(printf "%s\n" "${times[@]}" | sort -n) )
  min="${sorted[0]}"
  max="${sorted[$(( ${#sorted[@]} - 1 ))]}"
  n=${#sorted[@]}
  if (( n % 2 == 1 )); then
    med="${sorted[$(( n / 2 ))]}"
  else
    lo="${sorted[$(( n / 2 - 1 ))]}"
    hi="${sorted[$(( n / 2 ))]}"
    med=$(awk -v a="$lo" -v b="$hi" 'BEGIN { printf "%.6f\n", (a + b) / 2.0 }')
  fi
  mean=$(printf "%s\n" "${times[@]}" | awk '{ s += $1 } END { if (NR) printf "%.6f\n", s / NR; else print "0" }')
  echo "summary: n=${#times[@]} min=$min median=$med mean=$mean max=$max" >&2
fi
