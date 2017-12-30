{-# OPTIONS_GHC -Wall #-}

module TicTacToe where
import Framework
import GameBoardConnect
import Data.Coerce (coerce) -- safe coercion (e.g for newtype in either direction)
import Data.List (intersperse)

newtype TTTBoard = TTTBoard ConnBoard
unwrapTTT :: TTTBoard -> ConnBoard
unwrapTTT = coerce

instance GameBoard TTTBoard where
  turn       = turn . unwrapTTT
  judge      = judge . unwrapTTT
  validMoves = (map TTTBoard) . validMoves . unwrapTTT
  skipTurn   = TTTBoard . skipTurn . unwrapTTT

instance Show TTTBoard where
  show b = showB ++ "\nMoves: " ++ showM
    where
      showB :: String
      showB = show b'
      showM :: String
      showM = concat $ intersperse " " (map show (validMoves' isFree b'))
      b' :: ConnBoard
      b' = unwrapTTT b

emptyTTT :: TTTBoard
emptyTTT = TTTBoard $ empty 3 3 3

-- TODO: Add this to game board class?
safePlaceSimpleTTT :: Pos -> TTTBoard -> Maybe TTTBoard
safePlaceSimpleTTT p b = TTTBoard <$> safePlace isFree p (unwrapTTT b)
