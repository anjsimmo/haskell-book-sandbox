module Fib where
import Data.List

-- Inefficient implementation (but easiest to understand)
fib1 :: Integer -> Integer
fib1 0 = 0
fib1 1 = 1
fib1 n = fib1 (n - 1) + fib1 (n - 2)

fibs1 :: [Integer]
fibs1 = map fib1 [0..]

-- Efficient implementation that uses pairs to pass last 2 values
-- of sequence to next calculation.
fib2' :: Integer -> (Integer, Integer)
fib2' 0 = (1,0)       -- fib (-1),  fib 0
fib2' n = (b, a + b)  -- fib (n-1), fib n
  where (a, b) = fib2' (n - 1)

fib2 :: Integer -> Integer
fib2 = snd . fib2'

fibs2 :: [Integer]
fibs2 = map fib2 [0..]

-- Adapted from https://wiki.haskell.org/Memoization
fibs3 = 0 : 1 : map f [2..] where
  f n = fibs3 !! (n - 1) + fibs3 !! (n - 2)

-- Efficient implementation by rodneyp290
fibs4' :: Integer -> Integer -> [Integer]
fibs4' a b = a : fibs4' b (a + b)
fibs4 = fibs4' 0 1

-- Implementation using foldr (doesn't make things any simpler!)
-- Record to store the state of the accumulator (could have just used tuple instead)
data State = State { val     :: Integer   -- fib n
                   , restSeq :: [Integer] -- fib (n+2), fib (n+3), ...
                   }

fibs5 :: [Integer]
fibs5 = 0 : 1 : restSeq (foldr f base fibs5) where
  base :: State
  -- We never reach the end an infinite sequence, so base can be anything we like.
  base = undefined 
  -- Accumulator holds info about the fib value 1 place to the right of the current value.
  -- We use this to build up the rest of the sequence 2 places to the right of the current value.
  f :: Integer -> State -> State
  f x acc = State {val = x, restSeq = x + val acc : restSeq acc}

