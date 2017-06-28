{-# LANGUAGE ScopedTypeVariables #-}

module Hanoi where
import Data.Bool
import Data.Maybe

type Peg = String
type Move = (Peg, Peg)
type StackSize = Integer
type MoveCount = Integer
type Bound = (Integer, Integer)

-- Tabulate number of moves with 1 temp peg
-- TODO: Memoize
hanoi3Moves :: StackSize -> MoveCount
hanoi3Moves 0 = 0
hanoi3Moves n = hanoi3Moves (n - 1) * 2 + 1 -- move everything to temp peg, move base disk to target, move temp stack to target

-- Tabulate number of moves with 2 temp pegs
-- TODO: Memoize
hanoi4Moves :: StackSize -> MoveCount
hanoi4Moves 0 = 0
hanoi4Moves n = costFun4 substack4 substack3
  where (substack4, substack3) = pickStackSizes n

-- Number of moves required to solve by moving s4 disks to temp peg (using 2 temp pegs), s3 disks to target (using remaining 1 temp pegs), then s4 disks back to target
costFun4 :: StackSize -> StackSize -> MoveCount
costFun4 s4 s3 = 2 * hanoi4Moves s4 + hanoi3Moves s3

-- Find optimal split of temp tower (using 4 pegs) vs moving base pegs directly to target (using remaining 1 temp peg)
pickStackSizes :: StackSize -> (StackSize, StackSize)
pickStackSizes n = optimizeSplit n (0, (n-1)) costFun4 -- set bounds to exclude split n,0 to avoid infinite recursion

-- Find optimal split of stack c into two parts c = a + b, that minimises the cost of costFun(a,b)
optimizeSplit :: StackSize -> Bound -> (StackSize -> StackSize -> MoveCount) -> (StackSize, StackSize)
optimizeSplit c (minA, maxA) costFun = (a, c - a)
  where a = optimize (\a' -> costFun a' (c - a')) [minA..maxA]

-- find val that minimises cost funtion
-- needs scoped variables: https://stackoverflow.com/questions/5476378/how-to-reuse-a-type-variable-in-an-inner-type-declaration
optimize :: forall a. (a -> Integer) -> [a] -> a
optimize fc [] = error "Called optimize on an empty set of options"
optimize fc xs = fst $ fromMaybe (error "No minimum. Impossible when at least one option") $ optimize'
  where
    costsLabled :: [(a, Integer)]
    costsLabled = map (\x -> (x, fc x)) xs
    accMin :: (a, Integer) -> Maybe (a, Integer) -> Maybe (a, Integer)
    accMin p Nothing = Just p
    accMin (x, cost) (Just (curX, curCost)) = bool (Just (curX, curCost)) (Just (x, cost)) (cost < curCost)
    optimize' :: Maybe (a, Integer)
    optimize' = foldr accMin Nothing costsLabled

hanoi3 :: StackSize -> Peg -> Peg -> Peg -> [Move]
hanoi3 0 a b c = []
hanoi3 n a b c = hanoi3 (n - 1) a c b ++ [(a, b)] ++ hanoi3 (n - 1) c b a

hanoi4 :: StackSize -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d = (hanoi4 substack4 a c b d) ++ (hanoi3 substack3 a b d) ++ (hanoi4 substack4 c b a d)
  where (substack4, substack3) = pickStackSizes n

