{-# OPTIONS_GHC -Wall #-}

module GameBoardC4Util (estimateC4Util) where
import Framework
import GameBoardConnectUtil
import Connect4

estimateC4Util :: C4Board -> Util
estimateC4Util = estimateConnUtil . unwrapC4
