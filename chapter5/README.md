Typecheck for Tiger language
============================

## Overview

Semantic analysis contains an important phase: type check. It answers whether
your program is well-typed. For some languages, it can also do type inference.

The task in Chapter 5 is to write a type check for tiger language. Typecheck is
essentially an abstract interpreter of programs (on the syntax tree). The evaluation
of such an interpreter is to find the type as values.

## Environments

Tiger language has syntax of:
  * introducing types (even recursive types)
  * introducing variables

So we need to maintain two environments:
  * type env: map type id to the type struct
  * var env: map var id to type id

At the very beginning, the var env is empty and the type env contains built-in types
(int and string).

## Let Expression

Only `let expression` will enlarge the environments. `let expression` can:
1. define a new type
2. define a new variable
3. define a function

### Define Function

```
let function f (x: int) : int =
    x + 1
in
    ...
end
```

In the above example, to define a function, it will create two bindings:
1. var bindings :  `f`: `f_type_id`
2. type bindings:  `f_type_id`: `{function_type, int->int}`


