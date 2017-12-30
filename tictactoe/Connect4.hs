{-# OPTIONS_GHC -Wall #-}

module Connect4 where
import Framework
import GameBoardConnect
import Data.Coerce (coerce) -- safe coercion (e.g for newtype in either direction)
import Data.List (intersperse)

newtype C4Board = C4Board ConnBoard
unwrapC4 :: C4Board -> ConnBoard
unwrapC4 = coerce

instance GameBoard C4Board where
  turn       = turn . unwrapC4
  judge      = judge . unwrapC4
  validMoves = (map C4Board) . (validBoards isFreeC4') . unwrapC4
  skipTurn   = C4Board . skipTurn . unwrapC4

instance Show C4Board where
  show b = showB ++ "\nMoves: " ++ showM
    where
      showB :: String
      showB = show b'
      showM :: String
      showM = concat $ intersperse " " (map show (validMoves' isFreeC4' b'))
      b' :: ConnBoard
      b' = unwrapC4 b

emptyC4 :: C4Board
emptyC4 = C4Board $ empty 6 7 4

-- TODO: Add this to game board class?
safePlaceSimpleC4 :: Pos -> C4Board -> Maybe C4Board
safePlaceSimpleC4 p b = C4Board <$> safePlace isFreeC4' p (unwrapC4 b)

below :: Pos -> Pos
below (r, c) = (r + 1, c)

bottomRow :: ConnBoard -> Size
bottomRow b = rows b - 1

-- To go above the bottom row, make sure that there is a piece underneath
-- Note that isFree always returns False below the bottom row
isOntop :: ConnBoard -> Pos -> Bool
isOntop b p = not $ isFree b (below p)

-- isFreeC4 :: C4Board -> Pos -> Bool
-- isFreeC4 = isFreeC4' . unwrapC4

isFreeC4' :: ConnBoard -> Pos -> Bool
isFreeC4' b p = isFree b p && isOntop b p
