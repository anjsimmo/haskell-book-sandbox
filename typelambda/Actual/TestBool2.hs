-- Generated by testgen.sh
module Actual.TestBool2 where
import TypeLambda
import Data.Typeable
import Expected.TestBool as E
import System.IO

x :: [L(V(X,()))(L(V((),X))(L(V((),(),X))(Iff,(V(X,())),(V((),X)),(V((),(),X)))))]
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
