module Expected.TestZero where
import qualified TestUtil as U
import qualified TypeLambda as T

oracle :: T.Result -> Bool
oracle = U.testCases zeroTests zero'

selfTest :: Bool
selfTest = oracle zero'

showActual :: T.Result -> String
showActual = U.showCases zeroTests

showExpected :: String
showExpected = showActual zero'

zero' :: T.Result
zero' = T.I 0

zeroTests :: [[T.Result]]
zeroTests = [[]]
