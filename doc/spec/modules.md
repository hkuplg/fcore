# A Poor Man's Module System

Suppose we have something like this:

```
.
├── m1.sf
└── util
    ├── m3.sf
    └── simple
        └── m4.sf
```

m1.sf:
```
-- even and odd are from prelude

import util.m3;        -- fact impoted
import util.simple.m4; -- sub imported

{
print "The Fibonacci number of 3 - 1 is even? ";
even (fact (sub 3 1))
}

```

m3.sf:
```
package util

module {

  import util.simple.m4;

  multi (n : Int) (m : Int) = n * m;

  rec fact (n:Int) : Int =
     if n == 0 then 1 else multi n (fact (sub n 1))
}

```

m4.sf:
```
package util.simple

module {
  sub (a: Int) (b: Int): Int = a - b
}

```

In the root directory, type `f2j --run m1.sf`

```bash
$ f2j --run m1.sf
...
The Fibonacci number of 3 - 1 is even? true
```

## Program File and Module File ##

A module file is a `.sf` file where we can define a module like this:

```
module {
<definitions>
}
```

Note that, the module name is implicitly assumed to be the file name.

A definition is either recursive or non-recursive. Recursive
definitions begin with keyword *rec*; mutual recursive functions are
defined as `rec f ... and g ...;`.

The scope of a definition extends from where it is being defined to
the end of a module.

Any other file is a program file (as in `m1.sf`), which is like a main
method in Java.

**Beware**, normally we shouldn't try to run a module file, which may
or may not work. To test a module, just write a program file, and
import the module you want to test.


## Importing ##

To use a module in a program file, we use `import`. Note that import
expressions need to be put before any other expressions. Currently
`import` will import everything from a module, so there is no
information hiding whatsoever. :(

Modules can themselves import definitions from other modules, as in
`m3.sf`. If a local definition has the same name as one of the
definitions imported, the local one shadows the other.

When running a program file containing `import`s, the compiler will
recursively try to compile all the modules imported.


## Package and Namespace ##

In a module file, the package that this module belongs to is specified
with the `package` keyword, like this:

```
package foo

module {
...
}

```

At most one package declaration can appear in a module file. Package
names are written in all lower case. The name of the package must
match the directory structure where the corresponding module
resides. For example:

```
package foo.bar

module {
...
}
```

resides in `foo/bar/`.

To import a module of some package, you need to specify the full name
of the module.

## Prelude Module ##

The `prelude` module is imported by default into all program and
module files. Currently we have some simple definitions included. For
more information, look at directory `lib/predef`.


## Future Work ##

The current module system lacks some important features:

1. No datatypes and type synonyms are supported in a module file. (We
   are working hard to bring them back.)
2. Due to some murky parsing issues, the name of a module file must be
   in all lower case.
3. No information hiding. We are investigating an `exporting`
   mechanism.
