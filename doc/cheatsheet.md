# F2J Language Cheatsheet

Quick reference of language syntax

## Types

Type varibale names start with upper case letters.

Predefined types: `Int`, `String`, `Bool`, `Char`, `Float`, `Double`

They corresponds to Java classes with similar names. For example, `Int`
corresponds to `java.lang.Integer`. So you may call Java methods like:
`1.toString()`.

Function types: `Int -> String`

Product types: `(String, Int)`

Universal quantification: `forall A B. A -> B -> A`

Note that one `forall` can lead multiple type parameters as long as they are distinct.

Parameterized types (operator application): `ExprAlg[String]`, `Function1[A,B]`

Intersection types: `Int & Bool`

Record types: `{name:String, age:Int}`

## Expressions

Variable names start with lower case letters.

Boolean literals: `True`, `False`

Lambdas: `\(x: Int) (y: Int) -> x + y`

Type lambdas: `/\A -> \(x: A) -> x`

Function application: `f x`

Let expressions: `let const[A,B] (x: A) (y: B) = x; const[Int,String] 1`

Recursive let expressions: add `rec` after `let`

Mutually recursive let: `let f = ... and g = ...; e`

If: `if x == 0 then 1 else fact n * this (n - 1)`

* `if`'s are expressions.
* There is no need to wrap the predicate `x == 0` with parenthesis.
* The `then` keyword is mandatory.

Note that there must be a body, lead by a semicolon, following the bindings.

Tuple construction: `(1,2,3)`

There should be at least two items. Otherwise `(1)` just means `1`.

Tuple projection: `(1,2,3)._1` (index starts with 1, just like in Scala)

Tuple projection is type-safe: `(1,2)._3` won't compile.

Merge of two values: `1 ,, "hi"`

Record construction: `{name = "George", age = 21}`

Record projection: `let r = {name="George", age = 17}; r.age`

Record update: `let r = {name="George", age = 17}; r with {name="Nicole", age = 18}`

Local type synonyms: `type Arrow[A,B] = A -> B; ...`

Algebraic data types:
```
data BTree [A,B] = Leaf A
                 | Node BTree[A,B] B BTree[A,B];
Node[Bool,Int] (Leaf[Bool,Int] True) 7 (Leaf[Bool,Int] False)
```
You may leave out the final semicolon.

Expression sequences: `{1; 2; 3}`

Java construction: `new String("abc")`

Java method calls / field access: `myObject.hashCode()`, `"hello".concat(", world")`

The value of the sequence takes the last item, in the above case, `3`.

## Modules

```
module M {

};
...
```
Link modules with source file:
- f2ji: ```-l --module=MODULE```
- f2j: ```:link <sourceFile> -m <module1> <module2> ...```

F2ji:
- ```:load M``` Load module M into REPL environment, allow user to use functions defined in M, say M.add
- ```:import M``` Allow user to directly use add instead of M.add
