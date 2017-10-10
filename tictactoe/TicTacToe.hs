module TicTacToe where

import IO (gameLoop)
import qualified Data.Map as M
import Data.List (genericLength, sortBy)
import Data.Bool
import Data.Maybe
import Numeric.Natural
import Data.Ord

data Board = Board
  { rows :: Size
  , cols :: Size
  , connect :: Size
  , turn :: Turn
  , tokens :: M.Map Pos Token
  } deriving (Eq)
instance Show Board where
  show = prettyBoard

data Turn = AI | Human deriving (Eq, Show)
data Token = T Turn | Empty deriving (Eq)
instance Show Token where
  show = prettyCell

type Pos = (Size, Size)
type Size = Int
type Depth = Int
type Intelligence = Depth -- AI strength is tree depth to search
data BoardKnown = BoardKnown Board EndGame deriving (Show)
data EndGame = L | D | W deriving (Eq, Show, Ord)  -- Order as appears in data type. Ordered by utility to AI.
endGameUtil :: EndGame -> Float
endGameUtil L = 0.0
endGameUtil D = 0.5
endGameUtil W = 1.0

data Tree a = Tree a [Tree a] deriving (Show)
getRoot :: Tree a -> a
getRoot (Tree r _) = r
getChildren :: Tree a -> [Tree a]
getChildren (Tree _ cs) = cs

type GameTree = Tree Board

-- lazily generate game tree
genGameTree :: Board -> GameTree
genGameTree b = Tree b (map genGameTree (validBoards b))

data StateVal a v = StateVal a v
getState :: StateVal a v -> a
getState (StateVal s _) = s
getVal :: StateVal a v -> v
getVal (StateVal _ v) = v

trimTree :: Depth -> Tree a -> Tree a
trimTree 0 t = Tree (getRoot t) []
trimTree n t = Tree (getRoot t) (map (trimTree (n-1)) (getChildren t))

-- Suggested by Rhys as way to correctly sort games by utility
data Score = LoseIn Natural Float | Draw Float | WinIn (Down Natural) Float deriving (Show, Eq, Ord)

data GameUtil = GameUtil
  { endGame :: EndGame
  , depth   :: Depth    -- moves until endgame
  , avg     :: Float    -- avg util of children (in case human is not smart enough to see this deep)
  } deriving (Eq, Show)
instance Ord GameUtil where
  compare a b = compare (toScore a) (toScore b)

gameUtil :: EndGame -> GameUtil
gameUtil e = GameUtil { endGame = e, depth = 0 , avg = 0.0 }

toScore :: GameUtil -> Score
toScore g = case endGame g of
  L -> LoseIn (safeToNatural $ depth g) (avg g)
  D -> Draw (avg g)
  W -> WinIn (Down $ safeToNatural $ depth g) (avg g)

safeToNatural :: Depth -> Natural
safeToNatural x = fromIntegral (min 0 x) :: Natural

type BoardVal = StateVal Board GameUtil

evalGameTree :: GameTree -> Tree BoardVal
evalGameTree g = case v of
  Just v'  -> Tree (StateVal b (gameUtil v')) []
  Nothing  -> case (turn b) of
                AI     -> Tree (StateVal b maxEndGame { depth = depth maxEndGame + 1, avg = avgEndGame }) children'
                Human  -> Tree (StateVal b minEndGame { depth = depth minEndGame + 1, avg = avgEndGame }) children' -- Human win is AI loss (0 sum game)
  where
    b :: Board
    children :: [GameTree]
    Tree b children = g
    v :: Maybe EndGame
    v = judge b
    children' :: [Tree (StateVal Board GameUtil)]
    children' = map evalGameTree children
    maxEndGame :: GameUtil
    maxEndGame = safeMaximumEndGame $ map (getVal . getRoot) children'
    minEndGame :: GameUtil
    minEndGame = safeMinimumEndGame $ map (getVal . getRoot) children'
    avgEndGame :: Float
    avgEndGame = safeAvgEndGame $ map (getVal . getRoot) children'

safeMaximumNum     = maximum . ([0] ++)
safeMaximumEndGame [] = gameUtil D -- Treat invalid boards / trimmed nodes as draw
safeMaximumEndGame xs = maximum $ xs
safeMinimumEndGame [] = gameUtil D
safeMinimumEndGame xs = minimum $ xs
safeAvgEndGame [] = 0.0
safeAvgEndGame xs = sum (map (endGameUtil . endGame) xs) / genericLength xs

-- convenience function
reverseSortOn :: Ord b => (a -> b) -> [a] -> [a]
reverseSortOn f xs = sortBy (\a b -> compare (f b) (f a)) xs

pickBest :: Intelligence -> Board -> Board
pickBest i b = fromMaybe skip best
  where
    -- if can't go, do nothing other than toggling the turn
    skip = b { turn = toggleTurn (turn b) }
    best = case (judge b) of
      Nothing   -> pickBest' ((map getRoot) . getChildren . evalGameTree . (trimTree i) . genGameTree $ b)
      otherwise -> Nothing -- someone has already won

pickBest' :: [StateVal Board GameUtil] -> Maybe Board
pickBest' bs = listToMaybe . (map getState) $ opts -- pick the best (if any)
  where opts = reverseSortOn getVal bs -- first element is better

prettyBoard :: Board -> String
prettyBoard b = concat [prettyRow b r | r <- [0..(rows b)-1]]

prettyRow :: Board -> Size -> String
prettyRow b r = (concat $ map (\c -> prettyCell' b (r,c)) [0..(cols b)-1]) ++ "\n"

prettyCell' :: Board -> Pos -> String
prettyCell' b p = prettyCell $ M.findWithDefault Empty p (tokens b)

prettyCell :: Token -> String
prettyCell (T AI)    = " ✕"
prettyCell (T Human) = " ○"
prettyCell Empty     = " □"

place :: Pos -> Board -> Board
place p b = b { turn = toggleTurn (turn b), tokens = M.insert p (T $ turn b) (tokens b) }

safePlace :: Pos -> Board -> Maybe Board
safePlace p b = bool
  Nothing
  (Just $ place p b)
  (isFree b p)

placeAndRespond :: Intelligence -> Pos -> Board -> Maybe Board
placeAndRespond i p b = case (safePlace p b) of
  Nothing -> Nothing
  Just b -> Just $ (pickBest i b)

validMoves :: Board -> [Pos]
validMoves = validMoves'

validMoves' :: Board -> [Pos]
validMoves' b = filter (isFree b) [(r, c) | r <- [0..(rows b)-1], c <- [0..(cols b)-1]]

validBoards :: Board -> [Board]
validBoards b = map (\p -> place p b) (validMoves b)

isFree :: Board -> Pos -> Bool
isFree b p = (M.findWithDefault Empty p (tokens b) == Empty) && x >= 0 && x < (rows b) && y >= 0 && y < (cols b)
  where (x, y) = p

toggleTurn :: Turn -> Turn
toggleTurn AI = Human
toggleTurn Human = AI

judge :: Board -> Maybe EndGame
judge b
  | (longestRun (T AI) b)    >= (connect b)         = Just W
  | (longestRun (T Human) b) >= (connect b)         = Just L
  -- Equivalent to `validMoves b == []`, but faster
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

longestRun :: Token -> Board -> Size
longestRun t b = safeMaximumNum $ map (longestRun' t) $ map (select (tokens b)) $ genRCD (rows b) (cols b)

longestRun' :: (Eq a) => a -> [a] -> Size
longestRun' a = safeMaximumNum . (map genericLength) . runs a

select :: M.Map Pos Token -> [Pos] -> [Token]
select m = map (\p -> M.findWithDefault Empty p m)

empty :: Size -> Size -> Size -> Board
empty nR nC connect = Board { rows = nR, cols = nC, connect = connect, turn = Human, tokens = M.fromList [] }

main :: IO ()
main = gameLoop show show judge (placeAndRespond 3) (empty 4 4 3)
--main = gameLoop show show judge (placeAndRespond 3) (empty 3 3 3)
