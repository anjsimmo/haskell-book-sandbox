import Prelude hiding ((!!))
import Data.List.Safe ((!!))
import Control.Monad
import qualified Data.Set as S
import Data.List ((\\))

-- SEND + MORE == MONEY, S != 0, M != 0
-- Unique chars: DEMNORSY

data Soln = Soln { grabD :: Integer
                 , grabE :: Integer
                 , grabM :: Integer
                 , grabN :: Integer
                 , grabO :: Integer
                 , grabR :: Integer
                 , grabS :: Integer
                 , grabY :: Integer
                 }
instance Show Soln where
  show s = "SEND = " ++ (show . send) s ++ ", MORE = " ++ (show . more) s ++ ", MONEY = " ++ (show . money) s

goodCombs :: [Soln]
goodCombs = do
  d <- [0..9]
  e <- [0..9]
  guard $ noDups [d, e]
  m <- [1..9] -- non-zero
  guard $ noDups [d, e, m]
  n <- [0..9]
  guard $ noDups [d, e, m, n]
  o <- [0..9]
  guard $ noDups [d, e, m, n, o]
  r <- [0..9]
  guard $ noDups [d, e, m, n, o, r]
  s <- [1..9] -- non-zero
  guard $ noDups [d, e, m, n, o, r, s]
  y <- [0..9]
  guard $ noDups [d, e, m, n, o, r, s, y]
  let soln = Soln d e m n o r s y
  guard $ send soln + more soln == money soln
  pure soln

send  soln =                      grabS soln * 1000 + grabE soln * 100 + grabN soln * 10 + grabD soln
more  soln =                      grabM soln * 1000 + grabO soln * 100 + grabR soln * 10 + grabE soln
money soln = grabM soln * 10000 + grabO soln * 1000 + grabN soln * 100 + grabE soln * 10 + grabY soln

noDups :: (Ord a) => [a] -> Bool
noDups ns = S.size (S.fromList ns) == length ns

main :: IO ()
main = do
  let strs = map show goodCombs in
     mapM_ putStrLn strs
