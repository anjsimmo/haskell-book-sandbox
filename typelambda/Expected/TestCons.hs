module Expected.TestCons where
import qualified TestUtil as U
import qualified TypeLambda as T

oracle :: T.Result -> Bool
oracle = U.testCases consTests cons'

selfTest :: Bool
selfTest = oracle cons'

showActual :: T.Result -> String
showActual = U.showCases consTests

showExpected :: String
showExpected = showActual cons'

cons' :: T.Result
cons' = T.wrap2 cons''

cons'' :: T.Result -> T.Result -> T.Result
cons'' x xs = T.L $ x : T.unwrapList xs

consTests :: [[T.Result]]
consTests = [
    [T.I 1, T.L []],
    [T.L [], T.L []],
    [T.I 1, T.L [T.I 2, T.I 3]],
    [T.L [T.I 1, T.I 2], T.L [T.L [T.I 3, T.I 4], T.L [T.I 5, T.I 6]]]
  ]
