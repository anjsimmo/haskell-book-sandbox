module Expected.TestUnlines where
import Data.Char
import qualified TestUtil as U
import qualified TypeLambda as T

oracle :: T.Result -> Bool
oracle = U.testCases unlinesTests unlines'

selfTest :: Bool
selfTest = oracle unlines'

showActual :: T.Result -> String
showActual = U.showCases unlinesTests

showExpected :: String
showExpected = showActual unlines'

unlines' :: T.Result
unlines' = T.wrap1 unlines''

unlines'' :: T.Result -> T.Result
unlines'' = T.L . (map T.I) . (map ord) . unlines . (map $ (map chr) . (map T.unwrapInt)) . T.unwrapListList

unlinesTests :: [[T.Result]]
unlinesTests = map ((:[]) . T.L . (map $ T.L . (map T.I) . (map ord))) unlinesTests'

unlinesTests' :: [[String]]
unlinesTests' = [
    ["Hello", "world"]
  ]
