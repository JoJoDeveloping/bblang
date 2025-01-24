# Language Development Fun

This repository contains a bunch of things:
* An interpreter for a low-ish level language
* A parser for a high-ish level language
* A compiler from the high to the low level language

## Low-level language
The interpreter for this is "Miri"-inspired. It executes the code and reports when there is UB.
The low-level language itself is Basic Block based, but not SSA. 

## Memory model
The memory model is shared between both languages. It is block based, which each allocation being in its own block. Pointer arithmetic is only supported within this block.
Each block then has several cells, each of which can store a value. There are no "bytes".

## High-level language
The high-level language shares its values with the low-level lanugage, but it has expressions and structured control flow instead of basic blocks.
It also has names, instead of everything being just numbers.

It has a concrete syntax, and a parser. Here is an example program: 

```
fun fibrec(n) = if n < 2 then n else fibrec(n-1) + fibrec(n-2)
fun fibiter(n) locals i, j, t =
    set i = 0;
    set j = 1;
    while 0 < n do
        set t = i+j;
        set i = j;
        set j = t;
        set n = n - 1
    done;
    i
fun main() = fibiter(40)
```

Note that this high-level language is untyped and full of UB. For example, if you write a program adding two pointers, then you just have UB.
Eventually, I might build a fancy type system and support e.g. structs, or automatic drop. But until then, it is like a raw form of C.
