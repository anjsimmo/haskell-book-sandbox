# TypeLambda

A project to embed a program in a Haskell type signature.

## Dependencies:

* Haskell Stack
* m4
* make

## Running the Histogram example:

```
make run
```

## Why?

To win code golf! The [CIS 194 code golf assignment](https://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf) foolishly included the rule *Type signatures do not count towards the length of your solutions*.

My thought was to encode the assignment solution in the type signature itself, grab the type using [typeOf](http://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Typeable.html#v:typeOf), then evaluate the type signature. Unfortunately, for the assignment problems the implementation of the evaluator is longer than just directly writing the assignment solution out as an ordinary Haskell program. But in theory, for a harder problem my approach would have resulted in less lines of Haskell code (if you don't include the ridiculously long type signature).

## How?

Algebraic types allow us to construct arbitrarily complex type signatures from simple ones. I've defined a type `L` to represent a lambda expression, and used tuples to represent function application `(Func,Arg1,Arg2)`. I've made the list type `[Expression]` evaluate to just its internal `Expression` (so wrapping in the list type is effectively transparent to the evaluator).

```
data L a b deriving Typeable -- Lambda Param Body
data V a   deriving Typeable -- Val
data X     deriving Typeable
...
x :: [(L(V(X,()))(L(V((),X))(Iff,Integer,Int,(Minus,(V(X,())),(V((),X))))))]
```

Note that a simple empty list can still have a very long type!

```
x = []
```

We then reinterpret the type signature as a lambda calculus. The evaluator translates the types into primitives and function applications:

```
data Result = I Int | L [Result] | F (Result -> Result)
lam :: Result
lam = eval prims (typeOf x)
```

Technically, it's more of a Lisp than a lambda calculus, because in a true lambda calculus integers and lists themselves would be represented as nothing but lambda expressions. For my sanity, I've chosen to expose lists, integers and some primitive operations rather than attempting to represent everything as a lambda expression.

## Example Code:

I've translated a Haskell implementation of the `histogram` code golf assignment into a type lambda expression.

* The original Haskell implementation: [Golf.hs](Golf.hs) (copied from [rodneyp290](https://github.com/rodneyp290))
* The type lambda implementation: [Histogram.hs](Histogram.hs) (depends on [TypeLambda.hs](TypeLambda.hs))

Because directly writing programs as lambda expressions is incredibly tedious, I've used m4 macros to generate the expressions. The more human-friendly source used to generate the type lambda implementation is in [lib.m4](lib.m4). If you've never used m4 before, [this Linux Magazine article](http://www.linux-mag.com/id/163/) contains a gentle tutorial on the basics.

## Lessons Learned:

* Implementing a lambda evaluator in Haskell is simple (since it maps closely to the way function application in Haskell already works).
* Writing programs as lambda expressions is tedious.
* Debugging programs written as lambda expressions is a nightmare (large unreadable expressions with no static type checking). In order to debug my lambda expression, I ended up writing tests for each of the key parts (starting with the smallest, then working up towards more complex composite expressions). These tests can be run using `make test`.
