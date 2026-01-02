# ZmanLang

Inspired after reading Crafting Interpreters by Robert Nystrom, designed a custom high level language and assembly language and created a compiler, assembler, virtual machine and disassembler.  Compatible with WebAssembly (WASM) so it runs in the web browser (runs my VM on top of the WASM VM).  All implemented in low level C.  Used Github Copilot Agent to implement.  First time using


## Language, [ZmanLang](spec-lang.md)
A c-style, implictly typed, stort of toy language but relatively complete with function, arrays and strings.
Example:
```
print("Hello, World!\n");
```

## Assembly Language
32 bit, Stack based assembly language with support for directives and labels.  
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


## Assembler


## Virtual Machine






