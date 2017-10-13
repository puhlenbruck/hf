# hf
A Brainfuck interpreter written in Haskell

## Installation
There are currently no binaries available so to use hf you will need to build from source.  This project uses stack as the build tool.

To build this project, clone the git repository to your machine and use stack to build it.

```bash
git clone https://github.com/puhlenbruck/hf.git
cd hf
stack setup
stack build
```

now you can run the program using `stack exec -- hf-exe <arguments>`

To install use `stack install` instead of `stack build` which will copy the executable to a directory on the path (`$HOME/.local/bin` by default but depends on the local stack configuration).  If the target is not on the path stack will emit a warning to inform you.

## Usage
hf takes a file name as an argument to interpret as a brainfuck program.
Alternatively it reads a program as an argument to the `-c` flag

```
$ cat hello.bf
++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.

$ stack exec -- hf-exe hello.bf
Hello World!

$ stack exec -- hf-exe -c '++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.'
Hello World!
```
