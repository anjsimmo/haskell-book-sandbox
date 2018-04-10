# Reversesum

Given the total sum of x1 + x2 + ... + xn, find the combination of values x1, x2, ... xn that would result in the correct sum.

Example to calculate dice values that sum to 7:

```
$ stack ghc solver.hs
$ ./solver
Sum: (e.g. 7)
7
Split: (e.g. 2)
2
Valid values: (e.g. 1 2 3 4 5 6)
1 2 3 4 5 6
Calculating...
1 + 6
2 + 5
3 + 4
```

Example to reverse "anonymous" totals shown in an online research report:

```
$ stack ghc solver.hs
$ time ./solver
Sum: (e.g. 7)
494
Split: (e.g. 2)
4
Valid values: (e.g. 1 2 3 4 5 6)
0 132 16 115 16 50 104 178 135 150 71 36 59 28 123 183
Calculating...
104 + 123 + 132 + 135

real  0m15.426s
user  0m0.024s
sys   0m0.000s
```

```
$ stack ghc solver.hs
$ time ./solver
Sum: (e.g. 7)
582
Split: (e.g. 2)
4
Valid values: (e.g. 1 2 3 4 5 6)
0 132 16 115 16 50 104 178 135 150 71 36 59 28 123 183
Calculating...
71 + 150 + 178 + 183

real  0m7.756s
user  0m0.016s
sys   0m0.000s
```
