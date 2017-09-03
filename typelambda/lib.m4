define(`histogram',
`(
  L xs (
    concatenate,
      (unlines,
        (transpose,
          (hLines,
            (hInts, xs)
          )
        )
      ),
      legend
  )
)')dnl
define(`hLines',
`
(
  L xs (
    map, (
      hLine, (
        foldr,
          L x (
            L y (
              max, x, y
            )
          ),
          zero,
          xs
      )
    ),
    xs
  )
)
')dnl
define(`hLine', `(L h (L x (concatenate, (rep, (minus, h, x), space), (rep, x, asterix))))')dnl
define(`rep', `(L n (replicate, n))')dnl
define(`hInts', `(L xs (map, (count, xs), zeroToNine))')dnl
dnl the Y combinator (used to create recursive functions)
define(`Y',`(L f ((L x (f, (x, x))), (L x (f, (x, x)))))')dnl
dnl count is self referential. Use the Y combinator to pass the function (recursively applied to itself) as its own first argument.
define(`_count',
`
(
  L self (
    L xs (
      L n (
        bool,
          zero,
          (plus, (bool, zero, one, (eq, (head, xs), n)), (self, (tail, xs), n)),
          (isEmptyList, xs)
      )
    )
  )
)
')dnl
define(`count',`(Y, _count)')dnl
dnl length function is not actually used, other than as a simple test of recursive functions.
dnl length is self referential. Use the Y combinator to pass the function (recursively applied to itself) as its own first argument.
define(`_length',
`
(
  L self (
    L xs (
      bool,
        zero,
        (plus, one, (self, (tail, xs))),
        (isEmptyList, xs)
    )
  )
)
')dnl
define(`length',`(Y, _length)')dnl
dnl foldr is self referential. Use the Y combinator to pass the function (recursively applied to itself) as its own first argument.
define(`_foldr',
`
(
  L self (
    L f (
      L z (
        L xs (
          bool,
            z,
            (f, (head, xs), (self, f, z, (tail, xs))),
            (isEmptyList, xs)
        )
      )
    )
  )
)
')dnl
define(`foldr',`(Y, _foldr)')dnl
dnl if x-y == 0, then numbers are equal. Uses 0 to represent False
define(`eq',`(L x (L y (bool, one, zero, (minus, x, y))))')dnl
dnl replicate is self referential. Use the Y combinator to pass the function (recursively applied to itself) as its own first argument.
define(`_replicate',
`
(
  L self (
    L n (
      L x (
        bool,
          (cons, x, (self, (minus, n, one), x)),
          emptyList,
          (eq, n, zero)
      )
    )
  )
)
')dnl
define(`replicate',`(Y, _replicate)')dnl
dnl using a lambda for double rather than a direct substitution prevents length getting out of hand
define(`double',`(L x (plus, x, x))')dnl
define(`_2',`(plus, one, one)')dnl
define(`_3',`(plus, _2, one)')dnl
define(`_4',`(double, _2)')dnl
define(`_5',`(plus, _4, one)')dnl
define(`_6',`(double, _3)')dnl
define(`_7',`(plus, _6, one)')dnl
define(`_8',`(double, _4)')dnl
define(`_9',`(plus, _8, one)')dnl
define(`_10',`(double, _5)')dnl
define(`_20',`(double, _10)')dnl
define(`_30',`(plus, _20, _10)')dnl
define(`_40',`(double, _20)')dnl
define(`_50',`(plus, _40, _10)')dnl
define(`_60',`(double, _30)')dnl
dnl specific numbers that we need
define(`_32',`(plus, _30, _2)')dnl
define(`_42',`(plus, _40, _2)')dnl
define(`_48',`(plus, _40, _8)')dnl
define(`_49',`(plus, _40, _9)')dnl
define(`_51',`(plus, _50, one)')dnl
define(`_52',`(plus, _50, _2)')dnl
define(`_53',`(plus, _50, _3)')dnl
define(`_54',`(plus, _50, _4)')dnl
define(`_55',`(plus, _50, _5)')dnl
define(`_56',`(plus, _50, _6)')dnl
define(`_57',`(plus, _50, _7)')dnl
define(`_61',`(plus, _60, one)')dnl
define(`zeroToNine',
`
(cons, zero,
  (cons, one,
    (cons, _2,
      (cons, _3,
        (cons, _4,
          (cons, _5,
            (cons, _6,
              (cons, _7,
                (cons, _8,
                  (cons, _9, emptyList)
                )
              )
            )
          )
        )
      )
    )
  )
)
')dnl
define(`legend',
`
(unlines,
  (cons,
    (replicate, _10, chareq),
    (cons,
       (map,
          (plus, char0),
          zeroToNine
       ),
       emptyList
    )
  )
)
')dnl
dnl alternative definition of legend
define(`leg2',
`
(concatenate,
  (concatenate,
     (replicate, _10, chareq),
     (cons, newline, emptyList)
  ),
  (cons, char0,
    (cons, char1,
      (cons, char2,
        (cons, char3,
          (cons, char4,
            (cons, char5,
              (cons, char6,
                (cons, char7,
                  (cons, char8,
                    (cons, char9,
                      (cons, newline, emptyList)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
')dnl
dnl definition of map. Note that foldr is only partially applied (still expects a list as next parameter)
define(`map',`(L f (foldr, L x (cons, (f, x)), emptyList))')dnl
define(`concatenate',`(L xs (L ys (foldr, cons, ys, xs)))')dnl
define(`unlines',
`
(
  L xs (
    foldr,
      concatenate,
      emptyList,
      (map,
        L ys (
          concatenate, ys, (cons, newline, emptyList)
        ),
        xs
      )
  )
)
')dnl
dnl ASCII character codes
define(`newline',`_10')dnl
define(`space',`_32')dnl
define(`asterix',`_42')dnl
define(`char0',`_48')dnl
define(`char1',`_49')dnl
define(`char2',`_50')dnl
define(`char3',`_51')dnl
define(`char4',`_52')dnl
define(`char5',`_53')dnl
define(`char6',`_54')dnl
define(`char7',`_55')dnl
define(`char8',`_56')dnl
define(`char9',`_57')dnl
define(`chareq',`_61')dnl
dnl tmp vars (identified by arbitrary combinations of types)
define(`x',`(V (X,()))')dnl
define(`xs',`(V ((X,X),()))')dnl
define(`y',`(V ((),X))')dnl
define(`ys',`(V ((),(X,X)))')dnl
define(`z',`(V ((),(),X))')dnl
define(`n',`(V (X,X,X))')dnl
define(`h',`(V (X,X,X,X))')dnl
define(`f',`(V (X,X))')dnl
define(`self',`(V ((),()))')dnl
dnl builtins
define(`transpose',`Trans')dnl
define(`cons',`Cons')dnl
define(`head',`Car')dnl
define(`tail',`Cdr')dnl
define(`zero',`Int')dnl
define(`one',`Integer')dnl
define(`plus',`Plus')dnl
define(`minus',`Minus')dnl
define(`max',`Max')dnl
define(`bool',`Iff')dnl
dnl Iff also defined for lists
define(`isEmptyList',`(L x (bool, zero, one, x))')dnl
define(`emptyList',`()')dnl
define(`result',`histogram')dnl
dnl remove newlines and whitespace
define(`resultOneLine',`translit($1,`
 ')')dnl
define(`histogramcode',``-- Generated by codegen.sh
module Histogram where
import TypeLambda
import Data.Typeable
import Data.Char

x :: ['resultOneLine(result)`]
x = []

lam :: Result
lam = eval prims (typeOf x)

hist :: [Int] -> [Int]
hist xs = unwrapListInt $ unwrap1 lam (L $ map I xs)

histogram :: [Integer] -> String
histogram = (map chr) . hist . (map fromInteger)
'')dnl
