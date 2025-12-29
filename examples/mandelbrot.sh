#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ASM="$ROOT/bin/svm_asm"
VM="$ROOT/bin/svm_vm"
SRC="$ROOT/examples/mandelbrot.asm"
OUT="$ROOT/build/mandelbrot.zvm"
TMP="$ROOT/build/mandelbrot.tmp.asm"

WIDTH="${1:-80}"
HEIGHT="${2:-60}"
MAXITER="${3:-64}"

if [[ ! -x "$ASM" || ! -x "$VM" ]]; then
  echo "error: build tools not found; run 'make' first" >&2
  exit 2
fi

mkdir -p "$ROOT/build"

# Rewrite tunables into a temp asm.
# Keep it simple: replace whole .const lines.
sed -E \
  -e "s/^\\.const[[:space:]]+WIDTH[[:space:]]*=[[:space:]]*.*/.const WIDTH   = ${WIDTH}/" \
  -e "s/^\\.const[[:space:]]+HEIGHT[[:space:]]*=[[:space:]]*.*/.const HEIGHT  = ${HEIGHT}/" \
  -e "s/^\\.const[[:space:]]+MAXITER[[:space:]]*=[[:space:]]*.*/.const MAXITER = ${MAXITER}/" \
  "$SRC" > "$TMP"

"$ASM" "$TMP" "$OUT" >/dev/null
"$VM" "$OUT"
