module Expected.TestBool where
import qualified TestUtil as U
import qualified TypeLambda as T
import Data.Bool

oracle :: T.Result -> Bool
oracle = U.testCases boolTests bool'

selfTest :: Bool
selfTest = oracle bool'

showActual :: T.Result -> String
showActual = U.showCases boolTests

showExpected :: String
showExpected = showActual bool'

bool' :: T.Result
bool' = T.wrap3 bool''

bool'' :: T.Result -> T.Result -> T.Result -> T.Result
bool'' x y c = bool x y (trueish c)

trueish :: T.Result -> Bool
trueish (T.I 0)  = False
trueish (T.L []) = False
trueish _      = True

boolTests :: [[T.Result]]
boolTests = [
    [T.I 2, T.I 3, T.I 0],
    [T.I 2, T.I 3, T.I 1],
    [T.I 2, T.I 3, T.I 2],
    [T.I 2, T.I 3, T.L []],
    [T.I 2, T.I 3, T.L [T.I 2]]
  ]
