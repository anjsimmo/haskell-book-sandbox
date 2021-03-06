{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

-- Adapted from Rhys https://gitlab.com/rimmington/tictactoe/blob/master/IO.hs
module IO (gameLoop) where

import System.IO (hFlush, stdout)

inputLoop :: (a -> String) -> (a -> Maybe b) -> (String -> a -> Maybe a) -> a -> IO (a, b)
inputLoop shw won insert a0 = iterateUntilJust won' turn a0
  where
    won' a = (a,) <$> won a
    turn a = putStrLn (shw a) *> untilJust (tryInsert a =<< prompt)
    prompt = putStr "Enter a move: " *> hFlush stdout *> getLine
    tryInsert a coords
      | Just a' <- insert coords a = pure $ Just a'
      | otherwise                  = Nothing <$ putStrLn "That is not a valid move"
    -- Originally from Control.Monad.Loops
    untilJust :: (Monad m) => m (Maybe a) -> m a
    untilJust ma = iterateUntilJust id (const ma) Nothing
    iterateUntilJust :: (Monad m) => (a -> Maybe b) -> (a -> m a) -> a -> m b
    iterateUntilJust p f v
      | Just b <- p v = pure b
      | otherwise     = iterateUntilJust p f =<< f v

gameLoop :: (b -> String)                -- ^ How to show the board
         -> (r -> String)                -- ^ How to show the result
         -> (b -> Maybe r)               -- ^ Is there a game result?
         -> (String -> b -> Maybe b)     -- ^ Try to parse user input String and make move
         -> b                            -- ^ Initial board
         -> IO ()
gameLoop sb sr won add b0 = declare =<< inputLoop sb won add b0
  where
    declare (b, res) = putStrLn (sb b) *> putStrLn (sr res)
