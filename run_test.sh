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

run_one_zmc() {
	name="$1"
	src="examples/${name}.zm"
	asm="build/${name}.asm"
	zvm="build/${name}.zvm"
	out="build/${name}.stdout"
	exp="tests/expected/${name}_zmc.stdout"

	if [ ! -f "$src" ]; then
		echo "missing source: $src" >&2
		exit 2
	fi
	if [ ! -f "$exp" ]; then
		echo "missing expected output: $exp" >&2
		exit 2
	fi

	./bin/zmc "$src" "$asm"
	# Assembler status line is printed to stderr; silence it to keep test output clean.
	./bin/svm_asm "$asm" "$zvm" 2>/dev/null
	./bin/svm_vm "$zvm" >"$out"

	if diff -u "$exp" "$out"; then
		echo "ok: ${name}_zmc"
	else
		echo "FAIL: ${name}_zmc output mismatch" >&2
		exit 1
	fi
}

run_one_zmc_expect_trap() {
	name="$1"
	expect_code="${2:-}"
	expect_msg="${3:-}"
	src="examples/${name}.zm"
	asm="build/${name}.asm"
	zvm="build/${name}.zvm"
	out="build/${name}.stdout"
	err="build/${name}.stderr"

	if [ ! -f "$src" ]; then
		echo "missing source: $src" >&2
		exit 2
	fi

	./bin/zmc "$src" "$asm"
	# Assembler status line is printed to stderr; silence it to keep test output clean.
	./bin/svm_asm "$asm" "$zvm" 2>/dev/null

	set +e
	./bin/svm_vm "$zvm" >"$out" 2>"$err"
	rc=$?
	set -e

	if [ "$rc" -eq 0 ]; then
		echo "FAIL: ${name}_zmc expected trap, but exited 0" >&2
		exit 1
	fi

	if ! grep -q "VM TRAP:" "$err"; then
		echo "FAIL: ${name}_zmc expected VM TRAP message" >&2
		echo "--- stderr ---" >&2
		cat "$err" >&2
		exit 1
	fi

	if [ -n "$expect_code" ] && ! grep -q "code=${expect_code}" "$err"; then
		echo "FAIL: ${name}_zmc expected trap code ${expect_code}" >&2
		echo "--- stderr ---" >&2
		cat "$err" >&2
		exit 1
	fi

	if [ -n "$expect_msg" ] && ! grep -q "(${expect_msg})" "$err"; then
		echo "FAIL: ${name}_zmc expected trap msg '${expect_msg}'" >&2
		echo "--- stderr ---" >&2
		cat "$err" >&2
		exit 1
	fi

	if [ -s "$out" ]; then
		echo "FAIL: ${name}_zmc expected no stdout" >&2
		echo "--- stdout ---" >&2
		cat "$out" >&2
		exit 1
	fi

	echo "ok: ${name}_zmc_trap"
}

run_one_zmc_expect_compile_error() {
	name="$1"
	src="examples/${name}.zm"
	asm="build/${name}.asm"
	out="build/${name}.stdout"
	err="build/${name}.stderr"
	exp="tests/expected/${name}_zmc.stderr"

	if [ ! -f "$src" ]; then
		echo "missing source: $src" >&2
		exit 2
	fi
	if [ ! -f "$exp" ]; then
		echo "missing expected stderr: $exp" >&2
		exit 2
	fi

	set +e
	./bin/zmc "$src" "$asm" >"$out" 2>"$err"
	rc=$?
	set -e

	if [ "$rc" -eq 0 ]; then
		echo "FAIL: ${name}_zmc expected compile error, but exited 0" >&2
		exit 1
	fi

	if [ -s "$out" ]; then
		echo "FAIL: ${name}_zmc expected no stdout" >&2
		echo "--- stdout ---" >&2
		cat "$out" >&2
		exit 1
	fi

	if diff -u "$exp" "$err"; then
		echo "ok: ${name}_zmc_compile_error"
	else
		echo "FAIL: ${name}_zmc stderr mismatch" >&2
		exit 1
	fi
}

run_one test1
run_one test2
run_one test3

run_one_zmc hello
run_one_zmc concat
run_one_zmc ints
run_one_zmc control
run_one_zmc logical
run_one_zmc functions
run_one_zmc arrays
run_one_zmc arrays_zeroinit
run_one_zmc arrays_eq
run_one_zmc arrays_lit
run_one_zmc foreach
run_one_zmc kitchen_sink
run_one_zmc print_arrays
run_one_zmc implicit_return

# Array trap behavior (bounds/len checks): should trap with TRAP u16 code=1.
run_one_zmc_expect_trap arrays_oob 0x00000001 TRAP
run_one_zmc_expect_trap arrays_negidx 0x00000001 TRAP
run_one_zmc_expect_trap arrays_neglen 0x00000001 TRAP

# zmc compile-time error behavior
run_one_zmc_expect_compile_error const_assign