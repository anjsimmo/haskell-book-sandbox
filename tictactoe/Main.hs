{-# OPTIONS_GHC -Wall #-}

module Main where

import IO (gameLoop)
import GameBoardTTTUtil
import AIBruteForce
import TicTacToe
import Framework
import Data.Maybe

board :: TTTBoard
board = emptyTTT

level :: Intelligence
level = 3 -- level 0 => consider best move for AI,
          -- level 1 => consider Human reaction to AI move,
          -- level 2 => consider AI reaction to Human reaction to AI move

ai :: Ai TTTBoard TTTBoard
ai = bruteforce estimateTTTUtil level

-- Wraps ai (ensures that core ai can't cheet)
aiWrapper :: GameBoard b => Ai b b -> b -> b
aiWrapper fa b = fromMaybe d b'
  where b' = fa (validMoves b) id
        d  = skipTurn b

placeAndRespond :: Pos -> TTTBoard -> Maybe TTTBoard
placeAndRespond p b = case (safePlaceSimple p b) of
  Nothing -> Nothing
  Just b' -> Just $ (aiWrapper ai b')

main :: IO ()
main = gameLoop show show judge placeAndRespond board
