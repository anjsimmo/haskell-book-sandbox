-- Generated by testgen.sh
module Actual.TestZero2 where
import TypeLambda
import Data.Typeable
import Expected.TestZero as E
import System.IO

x :: [(Minus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),(Plus,Integer,(Plus,(Plus,Integer,Integer),Integer)))]
x = []

lam :: Result
lam = eval prims (typeOf x)

main :: IO ()
main = do
  putStrLn "Expected (Haskell): "
  putStrLn $ E.showExpected
  putStrLn "Actual (Lambda): "
  putStrLn $ E.showActual lam
  putStr "Test Result (Haskell ?= Lambda): "
  putStrLn $ show $ E.oracle lam
