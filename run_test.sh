#!/bin/sh

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$ROOT"

make -s release

run_one() {
	name="$1"
	asm="examples/${name}.asm"
	zvm="build/${name}.zvm"
	out="build/${name}.stdout"
	exp="tests/expected/${name}.stdout"

	if [ ! -f "$asm" ]; then
		echo "missing asm: $asm" >&2
		exit 2
	fi
	if [ ! -f "$exp" ]; then
		echo "missing expected output: $exp" >&2
		exit 2
	fi

	# Assembler status line is printed to stderr; silence it to keep test output clean.
	./bin/svm_asm "$asm" "$zvm" 2>/dev/null
	./bin/svm_vm "$zvm" >"$out"

	if diff -u "$exp" "$out"; then
		echo "ok: $name"
	else
		echo "FAIL: $name output mismatch" >&2
		exit 1
	fi
}

run_one test
run_one test2