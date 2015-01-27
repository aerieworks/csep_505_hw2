# csep_505_hw2
This homework is due on Friday, February 6 at 7pm.

In this homework, we'll build an interpreter for a language with functions, then add mutable state to it. We'll support a fairly rich surface syntax and use desugaring to translate down to a small core abstract syntax, with an initial environment that defines a collection of built-in constants and primitive operations.

We've pulled the definition of Result into its own module and implemented the Monad type class for it, so feel free to write your interpreters in monadic style and to use do notation if you're comfortable with that.

This time, we're also supplying a reference implementation of S-expression parsing.

The surface syntax we'll support for this assignment is as follows:

```
<e> ::= <number>
      | (if <e> <e> <e>)
      | x
      | (fun (x ...) <e>)
      | (<e> <e> ...)
      | (with* ([x <e>] ...) <e>)
```

We'll desugar this to the following core syntax:

```
<c> ::= <number>
      | (if <c> <c> <c>)
      | x
      | (fun (x) <c>)
      | (<c> <c>)
```

Desugaring of functions and applications uses currying to transform a multi-argument function (or application) into a single-argument one. For example:

```
(fun (g x)
  (g (g x)))
```

becomes:

```
(fun (g)
  (fun (x)
    (g (g x))))
```

and:

```
(f (+ 2 3) (if (< x y) x y))
```

becomes:

```
((f ((+ 2) 3)) (if ((< x) y) x y))
```

Our examples show desugaring in terms of the corresponding concrete syntax, but it is actually a function from elements of the Expr abstract syntax to elements of the CExpr ("core" expression) abstract syntax.

A with* expression is desugared into a function application:

```
(with* ([x (+ 1 2)]
        [y (* x x)]
        [x (+ x y)])
  (+ x y))
```

is equivalent to:

```
((fun (x)
   (with* ([y (* x x)]
           [x (+ y 3)])
     (+ x y)))
 (+ 1 2))
```

Continuing this unfolding, we get:

```
((fun (x)
   ((fun (y)
      ((fun (x)
         (+ x y))
       (+ y 3)))
    (* x x)))
 (+ 1 2))
```

The whole expression evaluates to 15.

In this example, we've left the binary operator calls in sugared form for the sake of simplicity. They would of course also need to be desugared.

Note that it should be an error to reuse a variable name in a multi-argument function; however (as in the example above), it is perfectly legal to rebind the same variable name in a with* expression. Also, a with* expression can have zero or more variable bindings (the zero case being equivalent to the body expression), but a function must have at least one argument, and hence an application must have at least two subexpressions. Your implementation of parseExpr may ignore such concerns and accept functions and applications with too few arguments, but desugar will need to raise an error, since there is no way to represent the corresponding expressions in the core syntax.


Problem 1. Download SExp.hs, Result.hs, and Expr.hs. (The file Token.hs is the same as in Homework 1.) Implement parseExpr, desugar, and checkIds in Expr.hs. You can probably reuse some parts of your parseExpr from Homework 1.

The function checkIds should check that all variable references in a core expression are bound and that no reserved identifier is ever rebound as a function variable. The function should return Ok () if these conditions hold and Err <reason> if not.

For example, we'd probably call checkIds with the names from the initial environment (bound) and the same names plus "if" and "fun" (reserved).


Problem 2. Download InterpFun.hs. Populate initialEnv with bindings for "true", "false", "+", "*", "=", and "<". The semantics of these operators should be the same as in Homework 1, except that they're curried now. We've provided a function called wrapBinaryArithOp to help define these primitives. Note that it won't raise an error until both arguments have been supplied, even if the first is of the wrong type.

Next, implement interp for this language. The language should be eager, and the semantics of if should be as in Homework 1.


Problem 3. Download InterpStore.hs. Make sure you understand the STR type. If you intend to take advantage of monads, supply appropriate implementations of the return and bind (>>=) operations for the STR monad instance. Regardless of whether you intend to use monadic style/operators, implement the functions alloc (which allocates a value to a fresh location), fetch (which returns the value at the given location), and update (which update the value at the given location).

Use these store functions to define primitives (PrimV instances) for functions called box (which creates a mutable cell), unbox (which returns the contents of a box), and set-box! (which updates the contents of a box). Note that set-box! takes two arguments: first the box, then the new value. These primitives should work on BoxV values, which have been added to the Val datatype.
Populate initialEnv with bindings for "box", "unbox", "set-box!", as well as all of the bindings from Problem 2. You may want to reuse/modify wrapBinaryArithOp to help with this.

Finally, implement interp for the language with boxes. (How much does the code differ from what you wrote in Problem 2?)


Handin mechanics. Submit your modified Expr.hs, InterpFun.hs, and InterpStore.hs in the dropbox for Homework 2. Don't forget to test your code.
