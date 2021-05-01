# mir9cc
## Overview

mir9cc is tiny C Compiler by Rust.
This compiler can interpret the some basic syntax of C. (See the test directory for details.)
<br>
<br>
mir9cc has some stages, and here is an overview of the internals.
<br>
1. Compiles an input program to AST.
2. Runs a semantic analyzer on the trees to add a type to each tree node and to confirm The input program is semantically correct.
3. Converts the trees to intermediate code (IR), which in some degree resembles x86-64 instructions but has an infinite number of registers.
4. Maps an infinite number of registers to a finite number of registers.
5. Generates x86-64 instructions from the IR.
<br>

## Run

Build.

    $ make mir9cc  
 
Run main test of `./test`.

    $ make test

Run example program(nqueen).

    $ ./target/debug/mir9cc examples/nqueen.c > tmp-nqueen.s
    $ gcc -static -o tmp-nqueen tmp-nqueen.s
    $ ./tmp-nqueen

## References
Great thanks to [9cc](https://github.com/rui314/9cc).
