{-# OPTIONS_GHC -Wall #-}

module GameBoardConnect where
import Framework
import qualified Data.Map as M
import Data.List (genericLength)
import Data.Bool

data Token = T Turn | Empty deriving (Eq)

data ConnSetup = ConnSetup
  { rows' :: Size
  , cols' :: Size
  , connect' :: Size
  } deriving (Eq)

data ConnBoard = ConnBoard
  { setup :: ConnSetup
  , connTurn :: Turn
  , tokens :: M.Map Pos Token
  } deriving (Eq)

rows :: ConnBoard -> Size
rows = rows' . setup
cols :: ConnBoard -> Size
cols = cols' . setup
connect :: ConnBoard -> Size
connect = connect' . setup

instance Show ConnBoard where
  show = prettyBoard

instance GameBoard ConnBoard where
  turn  = connTurn
  judge = judge'
  validMoves = validBoards isFree
  skipTurn b = b { connTurn = toggleTurn (connTurn b) }

prettyBoard :: ConnBoard -> String
prettyBoard b = concat [prettyRow b r | r <- [0..(rows b)-1]]

prettyRow :: ConnBoard -> Size -> String
prettyRow b r = (concat $ map (\c -> prettyCell' b (r,c)) [0..(cols b)-1]) ++ "\n"

prettyCell' :: ConnBoard -> Pos -> String
prettyCell' b p = prettyCell $ M.findWithDefault Empty p (tokens b)

prettyCell :: Token -> String
prettyCell (T AI)    = " ✕"
prettyCell (T Human) = " ○"
prettyCell Empty     = " ·"

place :: Pos -> ConnBoard -> ConnBoard
place p b = b { connTurn = toggleTurn (connTurn b), tokens = M.insert p (T $ connTurn b) (tokens b) }

safePlace :: (ConnBoard -> Pos -> Bool) -> Pos -> ConnBoard -> Maybe ConnBoard
safePlace isValid p b = bool
  Nothing
  (Just $ place p b)
  (isValid b p)

validMoves' :: (ConnBoard -> Pos -> Bool) -> ConnBoard -> [Pos]
validMoves' isValid b = filter (isValid b) [(r, c) | r <- [0..(rows b)-1], c <- [0..(cols b)-1]]

validBoards :: (ConnBoard -> Pos -> Bool) -> ConnBoard -> [ConnBoard]
validBoards isValid b = map (\p -> place p b) (validMoves' isValid b)

-- Correct for tic tac toe, will need to be further constrained for connect 4 (i.e. can only go on top of an existing piece)
isFree :: ConnBoard -> Pos -> Bool
isFree b p = (M.findWithDefault Empty p (tokens b) == Empty) && x >= 0 && x < (rows b) && y >= 0 && y < (cols b)
  where (x, y) = p

judge' :: ConnBoard -> Maybe EndGame
judge' b
  | (longestRun (T AI) b)    >= (connect b)         = Just W
  | (longestRun (T Human) b) >= (connect b)         = Just L
  -- Equivalent to `validMoves isFree b == []`, but faster
  | M.size (tokens b)        == (rows b) * (cols b) = Just D
  | otherwise                                       = Nothing

genRows :: Size -> Size -> [[Pos]]
genRows nR nC = [[(r,c) | r <- [0..(nR-1)]] | c <- [0..(nC-1)]]

genCols :: Size -> Size -> [[Pos]]
genCols nR nC = [[(r,c) | c <- [0..(nC-1)]] | r <- [0..(nR-1)]]

genLDiag :: Size -> Size -> Size -> Size -> [Pos]
genLDiag nR nC oR oC = [(oR+i,oC+i) | i <- [0..min (nC-oC-1) (nR-oR-1)]]

genLDiags :: Size -> Size -> [[Pos]]
genLDiags nR nC = [genLDiag nR nC r 0 | r <- [0..(nR-1)]] ++ [genLDiag nR nC 0 c | c <- [1..(nC-1)]]

mirrorLateral :: Size -> Pos -> Pos
mirrorLateral nR (r, c) = (nR-1-r, c)

genRDiags :: Size -> Size -> [[Pos]]
genRDiags nR nC = map (map (mirrorLateral nR)) (genLDiags nR nC)

genRCD :: Size -> Size -> [[Pos]]
genRCD nR nC = genRows nR nC ++ genCols nR nC ++ genLDiags nR nC ++ genRDiags nR nC

runs :: (Eq a) => a -> [a] -> [[a]]
runs _ [] = []
runs a l@(x:xs) = bool
  (runs a xs)
  ([takeWhile (==a) l] ++ runs a (dropWhile (==a) l))
  (x==a)

safeMaximumNum :: (Integral a) => [a] -> a
safeMaximumNum = maximum . ([0] ++)

longestRun :: Token -> ConnBoard -> Size
longestRun t b = safeMaximumNum $ map (longestRun' t) $ map (select (tokens b)) $ genRCD (rows b) (cols b)

longestRun' :: (Eq a) => a -> [a] -> Size
longestRun' a = safeMaximumNum . (map genericLength) . runs a

select :: M.Map Pos Token -> [Pos] -> [Token]
select m = map (\p -> M.findWithDefault Empty p m)

empty :: Size -> Size -> Size -> ConnBoard
empty nR nC con = ConnBoard { setup = ConnSetup { rows' = nR, cols' = nC, connect' = con }, connTurn = Human, tokens = M.fromList [] }
