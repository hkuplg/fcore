# How do I get set up? #

The toplevel Makefile sets up commands for running several of the most common tasks of the project, such as building the executable and running tests. To issue a command, run 
```
#!bash

make <name of task>
```
at the project root.

* `make parsers` will generate the parser as well the lexer for System F and other source languages, if any.
* `make test` will run all the tests inside the `test/` directory and the one at `compiler/TestSuite.hs`.
* `make compiler` will build the executable `f2j`. Note that you may invoke `f2j` (with the very name) anywhere in the file system, and it is *not* necessary to run `make parsers` before running this command.

# Using f2j #
Invoking `f2j` without any argument will show its usage. You may ask `f2j` to compile and run the Java code in the shell as soon as it is generated, by passing a `-r` or `--compile-and-run` flag.
