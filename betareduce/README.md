# Beta Reduce

Text-based lambda evaluator written to solve the examples from chapter 1 of the [Haskell Book](http://haskellbook.com/). Written for my own learning, and should definitely *not* be used for anything serious without further testing.

## Dependencies:

* Python 3

## Usage:

```
python3
from beta import beta_reduce
beta_reduce(r"(\z.zz)(\y.yy)")
```

Output:
```
step 0: (\z.zz)(\y.yy)
beta:   ([z := (\y.yy)]zz)
step 1: (\y.yy)(\y.yy)
alpha:  ([y<->a]\y.yy)(\y.yy)
step 2: (\a.aa)(\y.yy)
beta:   ([a := (\y.yy)]aa)
divergence detected
```

## How it's implemented:

Python string manipulations (TODO: [re-implement in Haskell](../betareduce2))

## Limitations:

* May crash or get the wrong answer when parameter names shadow parameters in the outer scope or free variables.
  * Instead of `(\x.(\x.x))(yy)`, input `(\x.(\m.m))(yy)`.
  * Instead of `(\x.(\y.yx))y`, input `(\x.(\m.mx))y`
  * `(\x.xx)(\x.xx)` is fine as is.
  * Steps will be automatically inserted to rename variables as needed to avoid introducing shadow parameters during beta reduction process.
* All variables used must be single letter lowercase ascii characters.
* Lambda expressions must be written out in full.
  * Instead of `\xy.xxyy`, inpput `(\x.(\y.xxyy))`
* Input is not validated. Entering invalid input (e.g. mismatched parentheses) may throw an exception, or return a garbage result.
* Doesn't remove redundant parentheses. E.g. may give `(\m.z(a))` instead of `(\m.za)`.

# Termination:

The beta_reduce function will return the answer when:
* The input is reduced to beta normal form

The beta_reduce function will return None or throw an exception when:
* The maximum number of steps allowed is exceeded
* The answer is divergent (due to the [halting-problem](https://en.wikipedia.org/wiki/Halting_problem) it is impossible to detect all forms of divergence).
* We run out of variable names

## Solving examples from Haskell Book:

`./test.py`

```
1.11 - "Normal Form or diverge?" exercises

Q1:
step 0: (\x.xxx)
beta normal form reached

Q2:
step 0: (\z.zz)(\y.yy)
beta:   ([z := (\y.yy)]zz)
step 1: (\y.yy)(\y.yy)
alpha:  ([y<->a]\y.yy)(\y.yy)
step 2: (\a.aa)(\y.yy)
beta:   ([a := (\y.yy)]aa)
divergence detected

Q3:
step 0: (\x.xxx)z
beta:   ([x := z]xxx)
step 1: zzz
beta normal form reached

1.11 - "Beta Reduce" exercises

Q1:
step 0: (\a.(\b.(\c.cba)))zz(\w.(\v.w))
beta:   ([a := z](\b.(\c.cba)))z(\w.(\v.w))
step 1: (\b.(\c.cbz))z(\w.(\v.w))
beta:   ([b := z](\c.cbz))(\w.(\v.w))
step 2: (\c.czz)(\w.(\v.w))
beta:   ([c := (\w.(\v.w))]czz)
step 3: (\w.(\v.w))zz
beta:   ([w := z](\v.w))z
step 4: (\v.z)z
beta:   ([v := z]z)
step 5: z
beta normal form reached

Q2:
step 0: (\x.(\y.xyy))(\a.a)b
beta:   ([x := (\a.a)](\y.xyy))b
step 1: (\y.(\a.a)yy)b
beta:   ([y := b](\a.a)yy)
step 2: (\a.a)bb
beta:   ([a := b]a)b
step 3: bb
beta normal form reached

Q3:
step 0: (\y.y)(\x.xx)(\z.zq)
beta:   ([y := (\x.xx)]y)(\z.zq)
step 1: (\x.xx)(\z.zq)
beta:   ([x := (\z.zq)]xx)
step 2: (\z.zq)(\z.zq)
alpha:  ([z<->a]\z.zq)(\z.zq)
step 3: (\a.aq)(\z.zq)
beta:   ([a := (\z.zq)]aq)
step 4: (\z.zq)q
beta:   ([z := q]zq)
step 5: qq
beta normal form reached

Q4:
step 0: (\z.z)(\z.zz)(\z.zy)
alpha:  ([z<->a]\z.z)(\z.zz)(\z.zy)
step 1: (\a.a)(\z.zz)(\z.zy)
beta:   ([a := (\z.zz)]a)(\z.zy)
step 2: (\z.zz)(\z.zy)
alpha:  ([z<->a]\z.zz)(\z.zy)
step 3: (\a.aa)(\z.zy)
beta:   ([a := (\z.zy)]aa)
step 4: (\z.zy)(\z.zy)
alpha:  ([z<->a]\z.zy)(\z.zy)
step 5: (\a.ay)(\z.zy)
beta:   ([a := (\z.zy)]ay)
step 6: (\z.zy)y
beta:   ([z := y]zy)
step 7: yy
beta normal form reached

Q5:
step 0: (\x.(\y.xyy))(\y.y)y
alpha:  ([y<->a]\x.(\y.xyy))(\y.y)y
step 1: (\x.(\a.xaa))(\y.y)y
beta:   ([x := (\y.y)](\a.xaa))y
step 2: (\a.(\y.y)aa)y
beta:   ([a := y](\y.y)aa)
step 3: (\y.y)yy
beta:   ([y := y]y)y
step 4: yy
beta normal form reached

Q6:
step 0: (\m.mm)(\b.ba)c
beta:   ([m := (\b.ba)]mm)c
step 1: (\b.ba)(\b.ba)c
alpha:  ([b<->c]\b.ba)(\b.ba)c
step 2: (\c.ca)(\b.ba)c
beta:   ([c := (\b.ba)]ca)c
step 3: (\b.ba)ac
beta:   ([b := a]ba)c
step 4: aac
beta normal form reached

Q7:
step 0: (\x.(\y.(\m.xm(ym))))(\x.z)(\x.a)
alpha:  ([x<->a]\x.(\y.(\m.xm(ym))))(\x.z)(\x.a)
step 1: (\a.(\y.(\m.am(ym))))(\x.z)(\x.a)
beta:   ([a := (\x.z)](\y.(\m.am(ym))))(\x.a)
step 2: (\y.(\m.(\x.z)m(ym)))(\x.a)
alpha:  ([x<->b]\y.(\m.(\x.z)m(ym)))(\x.a)
step 3: (\y.(\m.(\b.z)m(ym)))(\x.a)
beta:   ([y := (\x.a)](\m.(\b.z)m(ym)))
step 4: (\m.(\b.z)m((\x.a)m))
beta:   (\m.([b := m]z)((\x.a)m))
step 5: (\m.z((\x.a)m))
beta:   (\m.z(([x := m]a)))
step 6: (\m.z(a))
beta normal form reached
```
