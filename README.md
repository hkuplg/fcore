# F2J: A Compiler for FCore
[![License BSD 3][badge-license]](LICENSE)
[![Build Status](https://travis-ci.org/hkuplg/fcore.svg?branch=develop)](https://travis-ci.org/hkuplg/fcore)
[![Stories in Ready](https://badge.waffle.io/hkuplg/fcore.png?label=ready&title=Ready)](https://waffle.io/hkuplg/fcore)

## Building from Source

The following instructions should work on any platform, from OS X to
Ubuntu. It builds the compiler from source, thus may take some time.

1. Make sure you have installed the dependencies:

    + Install the [Haskell Platform](https://www.haskell.org/platform/)
    + Java SDK (8 or newer)
    + Apache Ant (version 1.8 or above)

2. Clone the [source] with [git]:

   ```bash
   git clone https://github.com/hkuplg/fcore.git
   cd fcore
   ```
[source]: https://github.com/hkuplg/fcore
[git]: http://git-scm.com/

3. Build and install:

   ```bash
   cabal update
   make
   ```

4. After the installation, invoking `f2j` in your console will show
   its usage. If not, you probably want to add `.cabal/bin` to your
   `$PATH`.

## Building on Windows ##

TODO

## Documentation ##

See `doc` directory for more details.


## Compilation Methods

`F2j` has a few built-in compilation methods (by default, it doesn't
use any optimization), namely `apply`, `stack` and `unbox`.

+ Apply: multi-argument optimization
+ Stack: tail call elimination
+ Unbox: auto-unboxing optimization

To use one or more of them, simply append the compilation methods you
want to use as the command line arguments.

For example, say you want to use the `apply` method, running the
following command:

    f2j -m apply some_file

If you want to combine different methods (say, `apply` and `stack`),
just type:

    f2j -m apply -m stack some_file

Finally, passing `-r` flag will make the compiler compile and run the
generated Java code.

## REPL

There is also a REPL at your service. Simply invoking `f2ji` will take
you to the REPL.

## Examples

In the `example` directory, you will see a lot of example programs
written in FCore. You may want to take a look at them to get familiar
with the syntax. These examples demonstrate different features of our
compiler, such as call-by-name, record syntax, modules, thunk, type
synonyms, etc.

Particularly, in `examples/fractals`, there is an interesting program
that draws a fractal.

## Troubleshooter

If you run into any problem, try to do

    make clean

and then

     make

If the problem persists, create an issue!

## License

BSD 3

See `LICENSE` at the top-level directory for details.

[badge-license]: https://img.shields.io/badge/license-BSD_3-green.svg
