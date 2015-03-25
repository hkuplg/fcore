# Expressions

## String literals

### String interpolation

Syntax: `\{e}` where `e` is an expression

Expansion:

Case 1: The type of the expression is of `String`.

```
let name = "Alice";
"I saw \{name} said hi to the crowd."
```

is expanded into:

```
let name = "Alice";
"I saw ".concat(name).concat(" said hi to the crowd")
```

Case 2: If the expression inside is of some Java class other than `String`, the
`toString` method will be implicitly called. For example:

```
let apples = 4;
"I have \{apples} apples"
```

is expanded into:

```
let apples = 4;
"I have ".concat(apples.toString()).concat(" apples")
```

Else: Type error
