# ZManLang -- Simple high level language specification

Statically typed but types are implicit, not explicit.  You don't declare type.

## Example code

```
print("Hello world!");

func fillArray(array[], value){
    let i := 0;
    while( i < length(array)){
        array[i] := value;
    }
}
const L :=5;
let data := [L]; 
fillArray(data, 3);

func arrayToText(data){
    let str := "[";
    let first :=true;
    foreach(v, data){
        if(first){
            first = false;
        }else{
            str := str + " ,";
        }
        str := str + text(v);
    }
    str := str + "]";
    return str;
}
print( arrayToText(data) );

func sum( array[] ) := { 
    let s := 0;
    foreach(v, array){
        s := s + v;
    }
    return s;
 } 
print("The sum is " + text( sum(data) ) );
```

## Variable Types

1. integer
2. string
3. boolean
3. function
4. [] array

## Keywords

- const
- let
- func 
- return
- if
- else
- while
- true 
- false
- foreach

## Operators

- Assignment Operator
  - :=
- Math Operators
  - +
  - -
  - /
  - *
  - %
- Comparison Operators
  - <
  - >
  - =
  - >=
  - <=
  - and
  - or
 - Expression Operators
()
 - Code Block Operators
{}
 - End of line operator
;
 - Comment operator
//
 - Array index operator
[]
 - String Concatenate Operator
+


## Built-in Functions
  - length(array) length of an array
  - input()
  - print(str)
  - number(str) turns text into a number
  - text(int) turns number into text

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

ZManLang v1 uses a simple bump allocator in linear memory.

#### Heap region

A program’s linear memory is conceptually divided into:

- **Static region**: data pre-initialized by the ZVM container’s `mem_init` bytes (e.g., string literal objects)
- **Heap region**: grows upward for dynamic objects (arrays, concatenations, `text()` results, input buffers)

The heap allocator maintains a single pointer `heap_ptr` that always points to the next free byte.

#### Alignment

All allocations are rounded up to 4-byte alignment.

#### No free/GC

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


## Flow of Control
 - if/else
 if ( a > b){
    x := 2;
 }else{
    x := 3;
 }
 - while
 while(i < 3){
    print( text(i) + "/n" );
    i := i +1;
 }

## Function definition
func add( a, b){
    return a+b;
}

## Variable Definition

const LIMIT := 3;
const NAME := "Fred";
let x := 3;

