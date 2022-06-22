# ghuloum_scheme
Following this [guide](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf) to incrementally build a Scheme compiler.

## Requirements
* gcc
* [Petite Chez Scheme v8.4](https://github.com/cisco/ChezScheme/)

## How to run?
To run the tests, simply run
```bash
make test
```

For each test case, the compiler produces an assembly file called `output.s`.

An included runtime written in C allows the generated assembly to be run correctly, it will be compiled and linked with the assembly file by using `gcc`. (Note: The below will be handled by the test driver)

```bash
# Compile generated code with runtime
gcc output.s runtime.c -o out

# Run resulting executable
./out
```
