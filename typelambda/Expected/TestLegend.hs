module Expected.TestLegend where
import Data.Char
import qualified TestUtil as U
import qualified TypeLambda as T

oracle :: T.Result -> Bool
oracle = U.testCases legendTests legend'

selfTest :: Bool
selfTest = oracle legend'

showActual :: T.Result -> String
showActual = U.showCases legendTests

showExpected :: String
showExpected = showActual legend'

legend' :: T.Result
legend' = T.L $ map T.I $ map ord $ "==========\n0123456789\n"

legendTests :: [[T.Result]]
legendTests = [[]]
