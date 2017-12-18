# Beta Reduce 2

Haskell implementation of a lambda calculus parser and evaluator.

## Usage:

```
stack runghc BetaParser.hs

Enter lambda expression to reduce:
(λx.x) y
(λx.x) y => y

Enter lambda expression to reduce:
(λf.λa.f a) (λx.x x) y
(λf.λa.f a) (λx.x x) y => y y

Enter lambda expression to reduce:
(λb.λt.λe.b t e) (λx.λy.x) x y
(λb.λt.λe.b t e) (λx.λy.x) x y => x

Enter lambda expression to reduce:
(λx.λy.y x) (y w)
(λx.λy.y x) (y w) => λy1.y1 (y w)

Enter lambda expression to reduce:
(λx.λx.x) x y
(λx.λx.x) x y => y

Enter lambda expression to reduce:
(λx.x) (λx.x) x
(λx.x) (λx.x) x => x
```

## Features:

* Parser understands conventions (application left associative, lambda abstractions with multiple variables in head), so no need for brackets everywhere.
* Can use `λ` or `\` interchangeably

## Limitations:

* Variables names must be single letter (which may be a foreign letter other than lambda), optionally followed by a number
* Doesn't detect divergence (unless a reduction step has no effect), so will hang on most divergent expressions.
