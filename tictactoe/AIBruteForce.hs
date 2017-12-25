{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module AIBruteForce ( bruteforce ) where
import Data.List (sortBy)
import Framework
import Data.Maybe

-- todo: implement as fold over game tree (game tree is a "rose tree")
evalGameTreeBf :: forall b. (GameBoard b) => (b -> Util) -> Tree b -> Tree (KnownUtil b)
evalGameTreeBf estimateUtil g = case v of
  Just v'  -> Tree (gb, endGameUtil v') []
  Nothing  -> case (turn gb) of
                AI     -> Tree (gb, maxEndGame) children'
                Human  -> Tree (gb, minEndGame) children' -- Human win is AI loss (0 sum game)
  where
    gb :: b
    cs :: [Tree b]
    Tree gb cs = g
    v :: Maybe EndGame
    v = judge gb
    children' :: [Tree (KnownUtil b)]
    children' = map (evalGameTreeBf estimateUtil) cs
    maxEndGame :: Util
    maxEndGame = safeMax (estimateUtil gb) $ map (util . root) children'
    minEndGame :: Util
    minEndGame = safeMin (estimateUtil gb) $ map (util . root) children'

-- safeMax takes first arg as what to be if empty list
safeMax :: (Ord a) => a -> [a] -> a
safeMax d [] = d
safeMax _ xs = maximum xs
safeMin :: (Ord a) => a -> [a] -> a
safeMin d [] = d
safeMin _ xs = minimum xs

-- convenience function
reverseSortOn :: Ord b => (a -> b) -> [a] -> [a]
reverseSortOn f xs = sortBy (\a b -> compare (f b) (f a)) xs

pickBest' :: [KnownUtil b] -> Maybe b
pickBest' bs = listToMaybe . (map node) $ opts -- pick the best (if any)
  where opts = reverseSortOn util bs -- first element is better

bruteforce :: (GameBoard b) => ComposeAI o b
bruteforce estimateUtil i os deref = pickBest' $ map (\o -> (o,) $ util $ root $ evalGameTreeBf estimateUtil $ (trimTree i) $ genGameTree $ deref o) os
