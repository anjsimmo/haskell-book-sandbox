module Expected.TestReplicate where
import qualified TestUtil as U
import qualified TypeLambda as T

oracle :: T.Result -> Bool
oracle = U.testCases replicateTests replicate'

selfTest :: Bool
selfTest = oracle replicate'

showActual :: T.Result -> String
showActual = U.showCases replicateTests

showExpected :: String
showExpected = showActual replicate'

replicate' :: T.Result
replicate' = T.wrap1 replicate''

replicate'' :: T.Result -> T.Result
replicate'' n = T.F (\x -> T.L (replicate (T.unwrapInt n) x))

replicateTests :: [[T.Result]]
replicateTests = [
    [T.I 1, T.I 0],
    [T.I 0, T.I 0],
    [T.I 0, T.F id],
    [T.I 2, T.I 1],
    [T.I 2, T.I 99],
    [T.I 2, T.L []],
    [T.I 2, T.L [T.I 3]],
    [T.I 2, T.L [T.I 3, T.L [T.I 1, T.I 2]]]
  ]
