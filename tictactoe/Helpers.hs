{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Helpers where

-- Generalised from GameBoardConnect
prettyBoard :: forall a b t. (Integral a, Integral b)
            => a -> b                     -- num rows, num columns
            -> ((a, b) -> Maybe t)        -- map
            -> String -> (t -> String)    -- default token, token representation
            -> String
prettyBoard nRows nCols m d prettyT = concat [prettyRow r | r <- [0..nRows-1]]
  where
    prettyRow :: a -> String
    prettyRow r = (concat $ map (\c -> prettyCell' (r,c)) [0..nCols-1]) ++ "\n"
    prettyCell' :: (a, b) -> String
    prettyCell' p = prettyCell $ m p
    prettyCell :: Maybe t -> String
    prettyCell (Just t) = prettyT t
    prettyCell Nothing  = d

-- Copied over from GameBoardConnectUtil
lerp :: Float -> Float -> Float -> Float -> Float -> Float
lerp ymin ymax xmin xmax x =  ymin + (x - xmin) * (ymax - ymin) / (xmax - xmin)

clamp :: Float -> Float -> Float -> Float
clamp xmin xmax x = max xmin (min xmax x)

clerp :: Float -> Float -> Float -> Float -> Float -> Float
clerp ymin ymax xmin xmax = (clamp ymin ymax) . (lerp ymin ymax xmin xmax)
