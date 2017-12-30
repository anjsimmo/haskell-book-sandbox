{-# OPTIONS_GHC -Wall #-}

module GameBoardChessUtil (estimateChessUtil) where
import Framework
import Helpers
import GameBoardChess
import qualified Data.Set as S

type Points = Int

estimateChessUtil :: ChessBoard -> Util
-- Need to ensure this is always >0 and <1 so that we don't value points over game outcome!
estimateChessUtil b = clerp 0.01 0.99 (fromIntegral minPoints) (fromIntegral maxPoints) (fromIntegral pointDiff)
  where
    pointDiff :: Points
    pointDiff = aiP - humanP
    maxPoints :: Points
    maxPoints = 8*1 + 3*2 + 3*2 + 3*2 + 9 -- could potentially have more points if multiple queens,
                                          -- but unlikely without suffering greater loss.
    minPoints :: Points
    minPoints = -maxPoints
    p :: S.Set Token -> Points
    p ts = sum $ map (points . tokenPiece) $ S.toList ts
    aiP :: Points
    aiP = p $ tokens (aiColor b) b
    humanP :: Points
    humanP = p $ tokens (toggleSide (aiColor b)) b

-- https://chess.stackexchange.com/questions/2409/how-many-points-is-each-chess-piece-worth
points :: Piece -> Points
points P = 1
points N = 3
points B = 3
points R = 3
points Q = 9
points K = 0 -- Irrelevant (can get king in checkmate, but never directly kill king)
