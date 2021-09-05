# The ℂ Programming Language
It's a language where the only types are "complex number" and "matrix of complex numbers". In particular, this means you cannot compare values except for exact equality. Good luck.

## What?
This project implements (a subset of) the ℂ programming language. Some sample files are located in `examples/`. The Cargo project produces LLVM IR and dumps it in `out/`, then executes it with a JIT.

## How?
Make sure you've installed LLVM 12 through your package manager. To fully compile source code:
1. `cargo run foo.ccc`
2. `llc out/foo.ll`
3. `as out/foo.s -o out/foo.o`
4. `gcc -no-pie -o out/foo out/foo.o`

I've only tested it on Arch with an AMD CPU. If it doesn't work for you, sorry.

## Why?
It seemed like a good idea at the time. ¯\\\_(ツ)\_/¯
