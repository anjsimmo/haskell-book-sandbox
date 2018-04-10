import Control.Monad
import Data.List (sort, (\\), intercalate)

allCombs :: (Eq a) => Int -> [a] -> [[a]]
allCombs 0 os  = [[]]
allCombs n os = (\o -> map (o :) (allCombs (n - 1) (os \\ [o]))) =<< os

goodCombs :: (Eq a) => Int -> ([a] -> Bool) -> [a] -> [[a]]
goodCombs n f os = mfilter f (allCombs n os)

ordered :: (Ord a) => [a] -> Bool
ordered ns = sort ns == ns

isGood :: Integer -> [Integer] -> Bool
isGood s ns = sum ns == s && ordered ns

main :: IO ()
main = do
  putStrLn "Sum: (e.g. 7)"
  s <- readLn
  putStrLn "Split: (e.g. 2)"
  n <- readLn
  putStrLn "Valid values: (e.g. 1 2 3 4 5 6)"
  options' <- getLine
  let options = map read (words options')
  putStrLn "Calculating..."
  let strs = map (intercalate " + " . (map show)) (goodCombs n (isGood s) (sort options))
  mapM_ putStrLn (strs)
