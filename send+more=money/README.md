# SEND+MORE=MONEY

```
    S E N D
+   M O R E
___________
= M O N E Y
```

Find integers *S*, *E*, *N*, *D*, *M*, *O*, *R*, *Y* such that the above equation holds true, subject to the constraint that each variable is unique and no leading zeros. <https://en.wikipedia.org/wiki/Verbal_arithmetic>

This is a brute force solution in Haskell, which uses the non-deterministic programming interpretation of lists to solve the problem. For efficiency, it's best to compile the program rather than run in GHCI. It takes approximately 40 seconds on my mid 2015 MacBook Pro.

```
$ stack ghc solver.hs
$ time ./solver
SEND = 9567, MORE = 1085, MONEY = 10652

real    0m40.853s
user    0m40.628s
sys     0m0.228s
```
