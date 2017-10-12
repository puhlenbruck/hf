# hf
A Brainfuck interpreter written in Haskell

## Usage
hf takes a file name as an argument to interpret as a brainfuck program.
Alternatively it reads a program as an argument to the `-c` flag

```
$ cat hello.bf
++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.

$ hf hello.bf
Hello World!

$ hf -c '++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.'
Hello World!
```
