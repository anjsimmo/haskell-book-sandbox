module Expected.TestFoldr where
import qualified TestUtil as U
import qualified TypeLambda as T
import qualified Expected.TestCons as TCons

oracle :: T.Result -> Bool
oracle = U.testCases foldrTests foldr'

selfTest :: Bool
selfTest = oracle foldr'

showActual :: T.Result -> String
showActual = U.showCases foldrTests

showExpected :: String
showExpected = showActual foldr'

foldr' :: T.Result
foldr' = T.wrap3 foldr''

foldr'' :: T.Result -> T.Result -> T.Result -> T.Result
foldr'' f x (T.L xs) = foldr f' x xs
  where
    f' :: T.Result -> T.Result -> T.Result
    f' = T.unwrap2 f
foldr'' _ _ _        = error "Incorrect types for foldr"

foldrTests :: [[T.Result]]
foldrTests = [
    [TCons.cons', T.L [], T.L []],
    [TCons.cons', T.L [], T.L [T.I 0, T.I 1, T.I 2]]
  ]
