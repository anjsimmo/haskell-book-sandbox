module Expected.TestEmptyList where
import qualified TestUtil as U
import qualified TypeLambda as T

oracle :: T.Result -> Bool
oracle = U.testCases emptyListTests emptyList'

selfTest :: Bool
selfTest = oracle emptyList'

showActual :: T.Result -> String
showActual = U.showCases emptyListTests

showExpected :: String
showExpected = showActual emptyList'

emptyList' :: T.Result
emptyList' = T.L []

emptyListTests :: [[T.Result]]
emptyListTests = [[]]
