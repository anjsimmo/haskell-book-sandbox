{-# OPTIONS_GHC -Wall #-}

module TicTacToe where
import Framework
import GameBoardConnect
import Data.Coerce (coerce) -- safe coercion (e.g for newtype in either direction)

newtype TTTBoard = TTTBoard ConnBoard
unwrapTTT :: TTTBoard -> ConnBoard
unwrapTTT = coerce

instance GameBoard TTTBoard where
  turn       = turn . unwrapTTT
  judge      = judge . unwrapTTT
  validMoves = (map TTTBoard) . validMoves . unwrapTTT
  skipTurn   = TTTBoard . skipTurn . unwrapTTT

instance Show TTTBoard where
  show = show . unwrapTTT

emptyTTT :: TTTBoard
emptyTTT = TTTBoard $ empty 3 3 3

-- TODO: Add this to game board class?
safePlaceSimpleTTT :: Pos -> TTTBoard -> Maybe TTTBoard
safePlaceSimpleTTT p b = TTTBoard <$> safePlace isFree p (unwrapTTT b)
