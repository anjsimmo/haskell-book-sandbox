-- Generated by testgen.sh
module Actual.TestLength where
import TypeLambda
import Data.Typeable
import Expected.TestLength as E
import System.IO

x :: [((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V((X,X),()))(Iff,Int,(Plus,Integer,((V((),())),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),())))))))]
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
