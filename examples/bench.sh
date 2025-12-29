#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ASM="$ROOT/bin/svm_asm"
VM="$ROOT/bin/svm_vm"
SRC="$ROOT/examples/bench.asm"
OUT="$ROOT/build/bench.zvm"
TMP="$ROOT/build/bench.tmp.asm"

ITER="${1:-20000000}"
REPS="${2:-1}"

if [[ ! -x "$ASM" || ! -x "$VM" ]]; then
  echo "error: build tools not found; run 'make' first" >&2
  exit 2
fi

mkdir -p "$ROOT/build"

# Rewrite ITER into a temp asm so you can benchmark different sizes without editing the source.
sed -E "s/^\\.const[[:space:]]+ITER[[:space:]]*=[[:space:]]*.*/.const ITER = ${ITER}/" "$SRC" > "$TMP"

"$ASM" "$TMP" "$OUT" >/dev/null

# Warm-up (reduces one-time cold-start effects)
"$VM" "$OUT" >/dev/null

echo "bench: ITER=$ITER REPS=$REPS" >&2

times=()

for ((i=1; i<=REPS; i++)); do
  echo "run $i:" >&2
  # /usr/bin/time writes to stderr; capture it while discarding the VM's stdout.
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
