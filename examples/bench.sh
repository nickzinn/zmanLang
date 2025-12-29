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

for ((i=1; i<=REPS; i++)); do
  echo "run $i:" >&2
  /usr/bin/time -p "$VM" "$OUT" >/dev/null
done
