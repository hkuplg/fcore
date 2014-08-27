# A brief intro to the compiler frontend #

The components: "parser"      "typechecker"      "desugarer"            "deinterer"
What you get:   --------> ESF -------------> ESF -----------> System FI -----------> System F
Representation:           E.Expr Name        E.Expr TcId      FI.Expr t e            F.Expr t e

Synonym convention:
E  -> ESF.Syntax
FI -> SystemFI.Syntax
F  -> SystemF.Syntax

Component     Location in the repo (relative to compiler/)

parser      | ESF/Parser.hs
typechecker | ESF/TypeCheck.hs
desugarer   | Desugar.hs
deinterer   | DeInter.hs

# (External / Non-cabal-specified) Dependencies #
GHC 7.6.x + cabal-install (put .cabal/bin in your path - e.g. PATH=$PATH:~/.cabal/bin/ )

JVM

ant 1.8.x and above

# How do I get set up? #

The toplevel Makefile sets up commands for running several of the most common tasks of the project, such as building the executable and running tests. To issue a command, run
```
#!bash

make <name of task>
```
at the project root.

* `make parsers` will generate the parser as well the lexer for System F and other source languages, if any.
* `make test` will run all the tests inside the `test/` directory.
* `make compiler` or simply `make` will build the executable `f2j`. Note that you may invoke `f2j` (with the very name) anywhere in the file system, and it is *not* necessary to run `make parsers` before running this command.

# Using f2j #
Invoking `f2j` without any argument will show its usage. You may ask `f2j` to compile and run the Java code in the shell as soon as it is generated, by passing a `-r` or `--compile-and-run` flag.
