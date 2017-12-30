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
import qualified GameBoardChess as C
import GameBoardChessUtil
import Text.Read (readMaybe)

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

aiC4 :: Ai C4Board C4Board
aiC4 = bruteforce estimateC4Util level3

-- Use lower AI search depth level for more complex games to prevent search space explosion.
aiChess :: Ai C.ChessBoard C.ChessBoard
aiChess = bruteforce estimateChessUtil level2

-- Wraps ai (ensures that core ai can't cheet)
aiWrapper :: GameBoard b => Ai b b -> b -> b
aiWrapper fa b = fromMaybe d b'
  where b' = fa ms id
        d  = skipTurn b
        ms = bool [] (validMoves b) (isNothing $ judge b)

placeAndRespond' :: (GameBoard b) => (p -> b -> Maybe b) -> Ai b b -> p -> b -> Maybe b
placeAndRespond' fsp fa p b = case (fsp p b) of
  Nothing -> Nothing
  Just b' -> Just $ (aiWrapper fa b')

placeAndRespond :: (GameBoard b, Read p) => (p -> b -> Maybe b) -> Ai b b -> String -> b -> Maybe b
placeAndRespond fsp fa s b = (\p -> placeAndRespond' fsp fa p b) =<< readMaybe s

placeAndRespondTTT :: String -> TTTBoard -> Maybe TTTBoard
placeAndRespondTTT = placeAndRespond safePlaceSimpleTTT aiTTT

placeAndRespondC4 :: String -> C4Board -> Maybe C4Board
placeAndRespondC4 = placeAndRespond safePlaceSimpleC4 aiC4

placeAndRespondChess :: String -> C.ChessBoard -> Maybe C.ChessBoard
placeAndRespondChess = placeAndRespond' C.safePlaceSimpleChess aiChess

mainTTT :: IO ()
mainTTT = gameLoop show show judge placeAndRespondTTT emptyTTT

mainC4 :: IO ()
mainC4 = gameLoop show show judge placeAndRespondC4 emptyC4

mainChess :: IO ()
mainChess = gameLoop show show judge placeAndRespondChess C.emptyChess

main :: IO ()
main = mainTTT
