-- Haskell Implementation of histogram (used as test oracle, as well as reference for converting to lambda calculus)
--
-- Adapted from:
-- https://github.com/rodneyp290/haskell-learnings/blob/13840aaa262cbde32ee7e65ade7ffd320cb7ef68/fp2/lecture03/Golf.hs

module Golf where
import Data.List
import Data.Bool

histogram :: [Integer] -> String
histogram xs = (unlines ( transpose ( hLines ( hInts xs))))
                ++ "==========\n0123456789\n"
--                ++  unlines [rep 10 '=', ['0'..'9']]

hLines :: [Integer] -> [String]
hLines xs = map (hLine (foldr (\x y -> max x y) 0 xs)) xs

hLine :: Integer -> Integer -> String
hLine h x = (rep (h-x) ' ') ++ (rep x '*')

rep :: Integer -> a -> [a]
rep n = replicate (fromInteger n)

hInts :: [Integer] -> [Integer]
hInts xs = map (count xs) [0..9]

count :: [Integer] -> Integer -> Integer
count [] _ = 0
count (h:t) n = (bool 0 1 (h==n)) + (count t n)


-- Simple implementation of builtin functions (used as reference for converting to lambda calculus)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x -> ((:) (f x))) []

concatenate' a b = foldr (:) b a

unlines' :: [String] -> String
unlines' xs = (foldr (concatenate') []) (map (++ "\n") xs)
