module TestUtil where
import qualified TypeLambda as T

showCases :: [[T.Result]] -> T.Result -> String
showCases cs f = foldr (\args s -> (showCase f args ++ '\n':s)) "" cs

showCase :: T.Result -> [T.Result] -> String
showCase f args = show $ applyArgs f args

testCases :: [[T.Result]] -> T.Result -> T.Result -> Bool
testCases cs f g = testEquiv cmpResult (applyArgs f) (applyArgs g) cs

testEquiv :: (b -> b -> Bool) -> (a -> b) -> (a -> b) -> [a] -> Bool
testEquiv eq f g = all (\x -> eq (f x) (g x))

applyArgs :: T.Result -> [T.Result] -> T.Result
applyArgs f args = foldl apply f args

apply :: T.Result -> T.Result -> T.Result
apply f g = T.unwrap1 f $ g

cmpResult :: T.Result -> T.Result -> Bool
cmpResult (T.I x)  (T.I y)   = x == y
cmpResult (T.L xs) (T.L ys)  = (length xs == length ys) && and (zipWith cmpResult xs ys)
cmpResult (T.F _)  (T.F _)   = error "can't cmp functions"
cmpResult _        _         = False
