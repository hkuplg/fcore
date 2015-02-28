# F2J Documentation

F2J is a research programming language (as well as its compiler) that compiles
to Java. It supports functional programming styles, interoperability with Java,
and intersection types.

## Core Language

### First Example

The syntax of the language resembles that of [OCaml](https://ocaml.org) and the
type system is built on top of polymorphic lambda calculus (System F). A piece
of code is worth a thousand words. Here is a program that computes the
[Manhattan distance](http://en.wikipedia.org/wiki/Taxicab_geometry) between two
points:

```ocaml
-- Comments start with '--'
-- src/Manhattan.sf
type Point A = { x: A, y: A };
let abs (x: Int) = if x >= 0 then x else (-1)*x;
let manhattanDistance (p1: Point Int) (p2: Point Int) =
  abs (p1.x - p2.x) + abs (p1.y - p2.y);
manhattanDistance { x = 0, y = 0 } { x = 3, y = 4 }
```

Now you should try it yourself! The above code is already included the `src`
directory of this Git repository. Fire up a terminal and type `f2j Manhattan.sf
-r`. (The `-r` option tells the compiler to run the program.) And you should see
something like:

```
$ f2j Manhattan.sf -r
Manhattan using [Naive]
Compiling to Java source code ( ./Manhattan.java )
7
```

In the first line of the program, we define a **type synonym** for points
with the `type` keyword, just for convenience. A point is a record containing
fields for the x- and y- coordinates. On the left-hand side of the equal sign,
the type has a type parameter `A`, which is then referred to on the right-hand
side. Therefore, `Point Int` is equivalent to `{ x: Int, y: Int }` and `Point
Double` to `{ x: Double, y: Double }`.

We then separate the type synonym definition with the rest of the program by a
semicolon (`;`). (But you shouldn't put one at the end of the program.)

On the next line, we define the absolute value function using the `let` keyword:

```ocaml
let abs (x: Int) = if x >= 0 then x else (-1)*x;
```

Each argument
should be put inside a pair of parentheses and its type explicitly annotated (in
this case, `Int`). As you might have expected, in functional languages, `if` is
an expression and you must provide both the `then` and `else` branch. Our
language is no exception.

The function should return an `Int`. You can also annotate the return type with
`: Int`:

```ocaml
let abs (x: Int): Int = if x >= 0 then x else (-1)*x;
```

but you don't need to do so (unless with `let rec`), as the compiler has type
inference built in.

It is also possible to move the parameters of the function to the right-hand side:

```ocaml
let abs = \(x: Int). if x >= 0 then x else (-1)*x;
```

Aha, I have shown you how to define anonymous functions (or lambdas) in F2J.

The rest of the program should be self-explanatory.

### Recursive Definitions

Recursive definitions are introduced by `let rec` and mutually-recursive
bindings are separated by the `and` keyword. This program tests if 42 is an odd
number:

```ocaml
-- src/EvenOdd.sf
let rec
  even (n: Int): Bool = if n == 0 then True else odd (n - 1)
and
  odd (n: Int): Bool  = if n == 0 then False else even (n - 1);
odd 42
```

Note that it is crucial that you supply the return type annotations for bindings
in `let rec` to aid the type checker (in this case, `: Bool`).

### Polymorphism

A polymorphic identity function returns anything that is passed to it. We may
define it in F2J below (`id`). It takes a type called `A` and a value called `x`
of type `A`, and returns `x` itself.

```ocaml
-- src/Poly.sf
let id A (x: A) = x;
let (id Int 1, id String "one")._2
```

The type checker will know `id` has the polymorphic type `forall A. A -> A`. In
order to pass a concrete value to `id`, you need to make it monomorphic by
passing a type of that concrete value. In the above, `id Int` is of type `Int ->
Int`. So `id Int 1` evaluates to `1`.

Note: `( ..., ... )` constructs a tuple and `._2` extracts the second element
from the tuple (1-indexed as in Scala).

## Additional Features

### Interopability with Java

You can use Java stuff seamlessly in F2J. In fact, `Int` is just
`java.lang.Integer` and `Bool` `java.lang.Boolean`. Besides, there are four
additional base types: `String`, `Char`, `Float`, and `Double`. You may call any
Java methods on values of those types. For example, the `java.lang.String` class
offers a `concat` method. So the following is a valid F2J program:

```ocaml
-- src/Interop.sf
"Hello, ".concat("world!")
```

However, overloaded operators are not allowed, such as:

```ocaml
-- src/OverloadedOperator.sf
"Hello, " + "world!"
```
