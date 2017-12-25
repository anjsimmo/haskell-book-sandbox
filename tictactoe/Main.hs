{-# OPTIONS_GHC -Wall #-}

module Main where

import IO (gameLoop)
import GameBoardTTTUtil
import GameBoardC4Util
import AIBruteForce
import TicTacToe
import Connect4
import Framework
import Data.Maybe
import Data.Bool

-- level 0 => consider best move for AI,
-- level 1 => consider Human reaction to AI move,
-- level 2 => consider AI reaction to Human reaction to AI move,
-- ...
level0 :: Intelligence
level0 = 0
level1 :: Intelligence
level1 = 1
level2 :: Intelligence
level2 = 2
level3 :: Intelligence
level3 = 3

aiTTT :: Ai TTTBoard TTTBoard
aiTTT = bruteforce estimateTTTUtil level3

-- N.b. use lower level for more complex games to prevent state space explosion.
aiC4 :: Ai C4Board C4Board
aiC4 = bruteforce estimateC4Util level3

-- Wraps ai (ensures that core ai can't cheet)
aiWrapper :: GameBoard b => Ai b b -> b -> b
aiWrapper fa b = fromMaybe d b'
  where b' = fa ms id
        d  = skipTurn b
        ms = bool [] (validMoves b) (isNothing $ judge b)

placeAndRespond :: GameBoard b => (Pos -> b -> Maybe b) -> Ai b b -> Pos -> b -> Maybe b
placeAndRespond fsp fa p b = case (fsp p b) of
  Nothing -> Nothing
  Just b' -> Just $ (aiWrapper fa b')

placeAndRespondTTT :: Pos -> TTTBoard -> Maybe TTTBoard
placeAndRespondTTT = placeAndRespond safePlaceSimpleTTT aiTTT

placeAndRespondC4 :: Pos -> C4Board -> Maybe C4Board
placeAndRespondC4 = placeAndRespond safePlaceSimpleC4 aiC4

mainTTT :: IO ()
mainTTT = gameLoop show show judge placeAndRespondTTT emptyTTT

mainC4 :: IO ()
mainC4 = gameLoop show show judge placeAndRespondC4 emptyC4

main :: IO ()
main = mainTTT
