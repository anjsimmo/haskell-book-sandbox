module Expected.TestCount where
import qualified Golf as G
import qualified TestUtil as U
import qualified TypeLambda as T

oracle :: T.Result -> Bool
oracle = U.testCases countTests count'

selfTest :: Bool
selfTest = oracle count'

showActual :: T.Result -> String
showActual = U.showCases countTests

showExpected :: String
showExpected = showActual count'

count' :: T.Result
count' = T.wrap2 count''

count'' :: T.Result -> T.Result -> T.Result
count'' (T.L xs) (T.I n) = T.I $ fromInteger $ G.count (map (toInteger . T.unwrapInt) xs) (toInteger n)
count'' _        _       = error "Wrong argument types for count"

countTests :: [[T.Result]]
countTests = [
    [T.L [], T.I 0],
    [T.L [T.I 1, T.I 0], T.I 0],
    [T.L [T.I 1, T.I 0], T.I 1],
    [T.L [T.I 1, T.I 0], T.I 2],
    [T.L [T.I 1, T.I 1, T.I 0, T.I 1], T.I 1]
  ]
