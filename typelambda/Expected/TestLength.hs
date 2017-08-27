module Expected.TestLength where
import qualified TestUtil as U
import qualified TypeLambda as T

oracle :: T.Result -> Bool
oracle = U.testCases lengthTests length'

selfTest :: Bool
selfTest = oracle length'

showActual :: T.Result -> String
showActual = U.showCases lengthTests

showExpected :: String
showExpected = showActual length'

length' :: T.Result
length' = T.wrap1 length''

length'' :: T.Result -> T.Result
length'' = T.I . length . T.unwrapList

lengthTests :: [[T.Result]]
lengthTests = map (:[]) lengthTests'

lengthTests' :: [T.Result]
lengthTests' = [
    T.L [],
    T.L [T.I 1],
    T.L [T.L []],
    T.L [T.I 1, T.I 0],
    T.L [T.I 0, T.I 0],
    T.L [T.L [], T.L []],
    T.L [T.I 2, T.L []],
    T.L [T.I 2, T.L [T.I 3]]
  ]
