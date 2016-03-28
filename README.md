# BfukC

BfukC is an optimizing Brainfuck compiler; it reads in a Brainfuck source file
and generates an optimized native code executable. Optionally it can run the
program directly using its built-in bytecode interpreter.

To produce native code executables BfukC produces LLVM assembly code, which it
compiles into optimized binary code using `clang`. This naturally means that in
order to
compile native code you need `clang` installed and in your `$PATH`.

The compiler uses the Brainfuck-specific optimization techniques mentioned
[here](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html), except
the scanloop and offset optizations. Due to aggressive optimization techniques
utilized by the `clang` LLVM compiler, these are not strictly necessary to produce
fast executables but they do shorten compilation times considerably due to Brainfuck's
extreme verbosity.

## Usage

Using the compiler is simple. For example, to compile and run the "Hello, world"
example found in `example` folder:

```sh
$ ./bfukc example/hello.bf
$ example/hello
Hello World!
```

To run the code directly:

```sh
$ ./bfukc -b example/hello.bf
Hello World!
```

## Installation

BfukC is written in OCaml using Jane Street's Core libraries. To compile it,
install `ocaml` and `opam` using your favorite package manager and then install
the required dependencies:

```sh
$ opam install core core_extended
```

After which you can compile the compiler:

```sh
$ corebuild bfukc.native
```

This will place the binary executable `bfukc.native` into `_build` subdirectory
(and also symlinks it to current directory).