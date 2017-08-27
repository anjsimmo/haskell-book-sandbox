module Expected.TestHistogram where
import Data.Char
import qualified Golf as G
import qualified TestUtil as U
import qualified TypeLambda as T

oracle :: T.Result -> Bool
oracle = U.testCases histogramTests histogram'

selfTest :: Bool
selfTest = oracle histogram'

showActual :: T.Result -> String
showActual = U.showCases histogramTests

showExpected :: String
showExpected = showActual histogram'

histogram' :: T.Result
histogram' = T.wrap1 histogram''

histogram'' :: T.Result -> T.Result
histogram'' = T.L . (map T.I) . (map ord) . G.histogram . (map toInteger) . T.unwrapListInt

histogramTests :: [[T.Result]]
histogramTests = map ((:[]) . T.L . (map T.I)) histogramTests'

histogramTests' :: [[Int]]
histogramTests' = [
    [],
    [0],
    [1],
    [9],
    [1,3],
    [7,9],
    [8,8],
    [0..9],
    [0..9] ++ [0..9] ++ [0,3],
    [1..9] ++ [2..9] ++ [3..9] ++ [4..9]
  ]
