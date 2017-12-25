{-# OPTIONS_GHC -Wall #-}

module GameBoardConnectUtil (estimateConnUtil) where
import Framework
import GameBoardConnect

-- TODO: Limit to only plausible runs (i.e. runs that are not blocked)
-- TODO: Consider multiple runs as better than one?
estimateConnUtil :: ConnBoard -> Util
estimateConnUtil b = clerp 0.0 1.0 (fromIntegral minPoints) (fromIntegral maxPoints) (fromIntegral points)
  where
    points = (longestRun (T AI) b) - (longestRun (T Human) b)
    minPoints = -(connect b)
    maxPoints = (connect b)

lerp :: Float -> Float -> Float -> Float -> Float -> Float
lerp ymin ymax xmin xmax x =  ymin + (x - xmin) * (ymax - ymin) / (xmax - xmin)

clamp :: Float -> Float -> Float -> Float
clamp xmin xmax x = max xmin (min xmax x)

clerp :: Float -> Float -> Float -> Float -> Float -> Float
clerp ymin ymax xmin xmax = (clamp ymin ymax) . (lerp ymin ymax xmin xmax)
