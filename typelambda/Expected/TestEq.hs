module Expected.TestEq where
import qualified TestUtil as U
import qualified TypeLambda as T
import Data.Bool

oracle :: T.Result -> Bool
oracle = U.testCases eqTests eq'

selfTest :: Bool
selfTest = oracle eq'

showActual :: T.Result -> String
showActual = U.showCases eqTests

showExpected :: String
showExpected = showActual eq'

eq' :: T.Result
eq' = T.wrap2 eq''

eq'' :: T.Result -> T.Result -> T.Result
eq'' (T.I x) (T.I y) = T.I $ bool 0 1 (x == y)
eq'' _       _       = error "eq only defined for integers"

eqTests :: [[T.Result]]
eqTests = [
    [T.I 1, T.I 1],
    [T.I 0, T.I 0],
    [T.I 3, T.I 3],
    [T.I 1, T.I 0],
    [T.I 0, T.I 1],
    [T.I 10, T.I 8],
    [T.I 8, T.I 10]
  ]
