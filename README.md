# ghuloum_scheme
Following this [guide](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf) to incrementally build a Scheme compiler.

## Requirements
* gcc
* [Petite Chez Scheme v8.4](https://github.com/cisco/ChezScheme/)

## How to run?
To compile the program
```bash
petite  --script main.scm
```

This produces an assembly file called `output.s`.

An included runtime written in C allows the generated assembly to be run
correctly, it can be compiled as so

```bash
# Compile generated code with runtime
gcc output.s runtime.c -o out

# Run resulting executable
./out
```
