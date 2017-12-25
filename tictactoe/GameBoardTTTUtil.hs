{-# OPTIONS_GHC -Wall #-}

module GameBoardTTTUtil (estimateTTTUtil) where
import Framework
import GameBoardConnectUtil
import TicTacToe

estimateTTTUtil :: TTTBoard -> Util
estimateTTTUtil = estimateConnUtil . unwrapTTT
