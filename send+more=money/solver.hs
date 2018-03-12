import Prelude hiding ((!!))
import Data.List.Safe ((!!))
import Control.Monad
import qualified Data.Set as S

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

allCombs :: [Soln]
allCombs = Soln <$> d <*> e <*> m <*> n <*> o <*> r <*> s <*> y
  where
    d = [0..9]
    e = [0..9]
    m = [1..9] -- non-zero
    n = [0..9]
    o = [0..9]
    r = [0..9]
    s = [1..9] -- non-zero
    y = [0..9]

goodCombs :: [Soln]
goodCombs = mfilter isGood allCombs

send  ns =                    grabS ns * 1000 + grabE ns * 100 + grabN ns * 10 + grabD ns
more  ns =                    grabM ns * 1000 + grabO ns * 100 + grabR ns * 10 + grabE ns
money ns = grabM ns * 10000 + grabO ns * 1000 + grabN ns * 100 + grabE ns * 10 + grabY ns

noDups :: Soln -> Bool
noDups ns = S.size (S.fromList [grabD ns, grabE ns, grabM ns, grabN ns, grabO ns, grabR ns, grabS ns, grabY ns]) == 8

isGood :: Soln -> Bool
isGood ns = noDups ns && send ns + more ns == money ns

main :: IO ()
main = do
  let strs = map show goodCombs in
     mapM_ putStrLn strs
