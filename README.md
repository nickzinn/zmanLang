# ZmanLang

Inspired over the holidays by reading [Crafting Interpreters](https://craftinginterpreters.com) by Robert Nystrom, I designed a custom high level language and assembly language and built a compiler, assembler, virtual machine, and disassembler. Compatible with WebAssembly (WASM) so it runs in the browser (VM compiled to WASM). All implemented in low level C. Built with help from GitHub Copilot Agent.  

I say inspired by Nystom's book, but of course I am old school and learned compiler design from the [Red Dragon book](https://en.wikipedia.org/wiki/Compilers:_Principles,_Techniques,_and_Tools).  The assembler code I learned back in the day on 6502 and Motorolla 68k.  

## Live demo

https://nickzinn.github.io/zmanLang/


## Language, [ZmanLang](spec-lang.md)
A C-style, implicitly typed, sort-of toy language but relatively complete with functions, arrays, and strings.
Example:
```
print("Hello, World!\n");
```

## Assembly Language, [StackVM-32 ASM](spec-assembler.md)
32-bit, stack-based assembly language with support for directives and labels.
Example:
```
.code
.entry main

main:
  PUSHI str_lit_0
  PUSHI 13
  SYSCALL 4
  HALT

.data

str_lit_0:
  .ascii "Hello world!\n"

.end
```

## Compiler

The v0 reference compiler, `zmc`, compiles ZmanLang source (`.zm`) to StackVM-32 assembly (`.asm`) (see [spec-lang.md](spec-lang.md) and [examples/web/README.md](examples/web/README.md)).


## Assembler

The assembler parses StackVM-32 assembly (`.asm`) and produces a runnable ZVM container (`.zvm`) (see [spec-assembler.md](spec-assembler.md) and [spec-vm.md](spec-vm.md)).


## Virtual Machine

The StackVM-32 virtual machine loads and executes `.zvm` containers (instruction set + container format: [spec-vm.md](spec-vm.md)).

## Disassembler

The disassembler turns a `.zvm` container back into readable assembly and auto-labels branch/call targets (see [spec-vm.md](spec-vm.md)).

## Build / Dependencies

- Required: a C compiler (`cc`, `clang`, or `gcc`) and `make`.
- Optional (static analysis): `clang-tidy` (preferred) or `clang` for `make lint`.
- Optional (web/WASM): Emscripten (`emcc`) for `make web`.
- Optional (web harness run): Python 3 for `python3 -m http.server`.

Common commands:

```bash
make release      # build optimized binaries into bin/
make debug        # debug build
make test         # run golden tests (also builds release)

make run PROG=examples/test1.asm
make disasm ZVM=program.zvm

make web          # build WebAssembly bundles (requires emcc)
```

## Deploy to GitHub Pages

This repo deploys the web harness in `examples/web` to GitHub Pages using a GitHub Actions workflow.

- One-time setup (repo settings): Settings → Pages → Source: GitHub Actions
- Deploys automatically on push to `main` (or you can manually run the workflow from the Actions tab)
- The workflow builds the WASM bundle (Emscripten) and publishes the contents of `examples/web`

Local build (optional):

```bash
make web
cd examples/web && python3 -m http.server 8000
```






