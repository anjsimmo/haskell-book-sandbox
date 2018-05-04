import Prelude hiding ((!!))
import Data.List.Safe ((!!))
import Control.Monad
import qualified Data.Set as S
import Data.List ((\\))
import Control.Monad.Trans.State.Lazy

-- SEND + MORE == MONEY, S != 0, M != 0
-- Unique chars: DEMNORSY

choice :: (Eq a) => [a] -> [(a, [a])]
choice s = map (\a -> (a, s \\ [a])) s

select :: (Eq a) => StateT [a] [] a
select = StateT choice

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

stateSolns :: StateT [Integer] [] Soln
stateSolns = do
  d <- select
  e <- select
  m <- select
  guard $ m /= 0 -- m is non-zero
  n <- select
  o <- select
  r <- select
  s <- select
  guard $ s /= 0 -- s is non-zero
  y <- select
  let soln = Soln d e m n o r s y
  guard $ send soln + more soln == money soln
  pure soln

send  soln =                      grabS soln * 1000 + grabE soln * 100 + grabN soln * 10 + grabD soln
more  soln =                      grabM soln * 1000 + grabO soln * 100 + grabR soln * 10 + grabE soln
money soln = grabM soln * 10000 + grabO soln * 1000 + grabN soln * 100 + grabE soln * 10 + grabY soln

goodCombs :: [Soln]
goodCombs = evalStateT stateSolns seed
  where
    seed = [0..9]

main :: IO ()
main = do
  let strs = map show goodCombs in
     mapM_ putStrLn strs
