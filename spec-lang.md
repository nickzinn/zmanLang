# ZManLang (v1) - Language Specification

ZManLang is a small, statically-typed language designed to compile to StackVM-32.

- Types are inferred (no explicit type syntax required in v1).
- Arrays are heap objects in VM linear memory.
- Strings are immutable heap objects in VM linear memory.

## Example program

```
print("Hello world!\n");

func fillArray(array[], value) {
  let i := 0;
  while (i < length(array)) {
    array[i] := value;
    i := i + 1;
  }
}

const L := 5;
let data := [L];
fillArray(data, 3);

func arrayToText(data) {
  let str := "[";
  let first := true;
  foreach(v, data) {
    if (first) {
      first := false;
    } else {
      str := str + ", ";
    }
    str := str + text(v);
  }
  str := str + "]";
  return str;
}

print(arrayToText(data) + "\n");

func sum(array[]) {
  let s := 0;
  foreach(v, array) {
    s := s + v;
  }
  return s;
}

print("The sum is " + text(sum(data)) + "\n");
```

## Lexical structure

### Whitespace

Spaces, tabs, and newlines may appear between tokens and are ignored.

### Comments

`//` starts a comment that runs until end-of-line.

### Identifiers

Identifiers match `[A-Za-z_][A-Za-z0-9_]*` and are case-sensitive.

### Keywords

The following are reserved:

- `const`, `let`, `func`, `return`, `if`, `else`, `while`, `foreach`
- `true`, `false`, `and`, `or`, `not`

### Literals

- Integer literal: base-10 digits with optional leading `-`.
  - Must fit signed 32-bit range.
- Boolean literals: `true`, `false`.
- String literal: double-quoted `"..."`.
  - Required escapes: `\"`, `\\`, `\n`, `\t`.

## Types

ZManLang v1 supports these types:

- `integer` (signed 32-bit)
- `boolean`
- `string`
- `array[integer]` (0-based, mutable)
- `function`

Types are inferred. The optional `[]` suffix on a binding name (for example `func f(a[], b) { ... }`) is a v1 type hint that constrains the binding to be an array type.

## Syntax (EBNF)

```
program        := { top_item } EOF
top_item       := func_def | statement

statement      := let_stmt
               | const_stmt
               | assign_stmt
               | if_stmt
               | while_stmt
               | foreach_stmt
               | return_stmt
               | expr_stmt

let_stmt       := "let" binding ":=" expr ";"
const_stmt     := "const" binding ":=" expr ";"

binding        := identifier [ "[" "]" ]

assign_stmt    := lvalue ":=" expr ";"
lvalue         := identifier
               | expr "[" expr "]"

if_stmt        := "if" "(" expr ")" block [ "else" block ]
while_stmt     := "while" "(" expr ")" block
foreach_stmt   := "foreach" "(" identifier "," expr ")" block
return_stmt    := "return" expr ";"
expr_stmt      := expr ";"

block          := "{" { statement } "}"

func_def       := "func" identifier "(" [ params ] ")" func_body
params         := param { "," param }
param          := binding
func_body      := block | ":=" expr ";"
```

## Operators and precedence

Precedence (highest to lowest):

1. Postfix: call `f(...)`, index `a[i]`
2. Unary: `-x`, `not x`
3. `* / %`
4. `+ -`
5. `< > <= >=`
6. `=`
7. `and` (short-circuit)
8. `or` (short-circuit)

Type rules (v1):

- `+ - * / %` operate on `integer` and produce `integer`, except `+` also supports `string + string`.
- `< > <= >=` operate on `integer` and produce `boolean`.
- `=` produces `boolean`.
  - For `string` and arrays, `=` compares pointer identity in v1.

## Built-in functions

Surface built-ins:

- `length(x)` -> `integer`
  - If `x` is an array, returns number of elements.
  - If `x` is a string, returns number of bytes.
- `print(str)` -> `integer`
  - Writes the string to stdout.
- `input()` -> `string`
  - Reads a line from stdin (see runtime notes).
- `number(str)` -> `integer`
  - Parses a base-10 integer (see runtime notes).
- `text(i)` -> `string`
  - Converts an integer to a base-10 string.

## Runtime Memory Model & Allocation

ZManLang programs execute on StackVM-32 and use the VM’s linear memory (`mem`) to store all non-scalar runtime objects.

### Value Representation

All values that live on the VM value stack are 32-bit words:

- `integer`: 32-bit signed (two’s complement)
- `boolean`: 0 = false, 1 = true
- `string`: 32-bit pointer (byte address into linear memory)
- `array`:  32-bit pointer (byte address into linear memory)
- `function`: 32-bit absolute code address (byte address into code section)

### Object Layouts in Linear Memory

All multi-byte fields are little-endian.

#### String object

A string value is a pointer `p` to the start of the string header:

- `u32 len` at `mem[p + 0 .. p + 3]`  (number of bytes)
- `u8 data[len]` at `mem[p + 4 .. p + 4 + len - 1]`

Strings are immutable. String concatenation allocates a new string.

#### Array object

An array value is a pointer `p` to the start of the array header:

- `u32 len` at `mem[p + 0 .. p + 3]`  (number of elements)
- `u32 elems[len]` at `mem[p + 4 .. p + 4 + (len*4) - 1]`

Arrays are mutable. Indexing uses 0-based indices.

- load `array[i]`:
  - bounds check: `0 <= i < len`
  - element address: `p + 4 + i*4`
- store `array[i] := v`:
  - same bounds check
  - store 32-bit element value at element address

### Allocation Strategy (v1)

ZManLang v1 uses the StackVM-32 bump allocator in linear memory.

The VM initializes the heap pointer to the first 4-byte aligned address at or above `MemInitSize` (the size of the container's initial memory image).

Allocation is performed via:

- `SYSCALL 6` `heap_alloc(nbytes) -> ptr` (4-byte aligned; traps on out-of-memory)
- `SYSCALL 7` `heap_ptr() -> ptr` (optional; current bump pointer)

All allocations are rounded up to 4-byte alignment.

There is no `free` and no garbage collection in v1. All allocations live for the duration of the program.

### String Literals

String literals may be emitted into the ZVM container’s `mem_init` region as pre-built String objects (header + bytes). In that case, a string literal expression evaluates to the literal object pointer.

### Built-in Functions on This Memory Model

- `length(array)` returns `u32 len` from the array header at offset 0
- `length(str)` returns `u32 len` from the string header at offset 0
- `print(str)` writes `len` bytes from `str.data` (i.e., pointer `p+4`) to stdout
- `input()` allocates a string buffer and reads bytes from stdin into it, then sets the string length
- `text(int)` allocates a new string containing the decimal ASCII representation
- `number(str)` parses decimal ASCII bytes from `str.data`

### Required Runtime Checks

- Array bounds check on every `array[index]` load/store (trap on failure)
- Division/modulo by zero traps (VM already traps)


## Compilation to StackVM-32 (codegen requirements)

This section defines the minimum code generation and ABI conventions needed to compile ZManLang to StackVM-32 assembly.

### Calling convention

ZManLang uses StackVM-32's standard calling convention:

- The caller evaluates and pushes arguments left-to-right.
- The caller invokes the callee with `CALL <addr32>` (or `CALLI`).
- The callee allocates locals with `ENTER nlocals`.
- The callee returns with `RET argc`.

Given a function with `argc` parameters, parameter `i` (0-based, left-to-right) is located at:

- `fp + offset(i)` where `offset(i) = -(2 + argc - i)`

Locals live at `fp + 0`, `fp + 1`, ... in the order chosen by the compiler.

### Expression evaluation convention

Compiling an expression must leave exactly one 32-bit value on the VM operand stack.

Compiling a statement must leave the operand stack height unchanged (any temporary values must be popped).

Short-circuit `and`/`or` must not evaluate the RHS when the result is already determined.

### Variables

#### Locals

Local bindings (`let`/`const` inside a function) are stored in FP-relative slots:

- Read: `LDFP <slot>`
- Write: `STFP <slot>`

Assigning to a `const` binding must be rejected at compile time.

#### Top-level (globals)

Top-level `let`/`const` bindings must be supported.

Recommended representation: allocate one 32-bit cell per global in the ZVM `mem_init` image (the `.data` section), and store the global's runtime value in that cell.

- Load global: `PUSHI <addr>; LOAD32`
- Store global: `PUSHI <addr>; <value>; STORE32`

### Arrays

Arrays use the runtime object layout defined earlier in this document.

Array allocation `[n]` must:

1. Evaluate `n`.
2. Trap if `n < 0`.
3. Compute bytes = `4 + n*4`.
4. Allocate with `SYSCALL 6`.
5. Write length at offset 0.
6. Ensure elements are 0-initialized.

Index load/store must bounds-check on every access and trap on failure.

### Strings

Strings use the runtime object layout defined earlier in this document.

#### Printing

To print a string value `p`:

- Load `len = mem[p+0..p+3]`
- Compute `data_ptr = p + 4`
- Call `SYSCALL 4` with `data_ptr` and `len`

#### Concatenation

`string + string` must allocate a new string object and copy bytes.

Recommended implementation: emit a runtime helper that allocates `4 + (len_a + len_b)` bytes via `SYSCALL 6`, stores the new length, then uses `MEMCPY` to copy both byte ranges.

### Built-ins

The compiler must implement the surface built-ins in one of these ways:

- inline code sequences, or
- emitted runtime helper functions in generated assembly.

Minimum behavior requirements:

- `length(array)` returns the `u32 len` field.
- `length(str)` returns the `u32 len` field.
- `print(str)` writes the string bytes to stdout.

`input()` (v1 deterministic): reads a single line from stdin.

- Stops at `\n` or EOF.
- Does not include the trailing `\n` in the returned string.
- Reads at most 4096 bytes.

`number(str)` parses base-10 signed integers:

- optional leading `-`
- then one or more digits
- stops at the first non-digit
- returns 0 if there are no digits
- traps on overflow outside signed 32-bit range

`text(i)` returns a newly allocated string containing the base-10 ASCII representation of `i`.

### Diagnostics

Compile-time errors include:

- undefined identifiers
- assigning to `const`
- type errors (wrong operand types, wrong condition type, wrong argument count)

Runtime traps include:

- array bounds violations
- negative array length
- conversion overflow in `number(str)`

