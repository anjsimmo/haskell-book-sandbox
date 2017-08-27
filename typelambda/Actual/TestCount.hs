-- Generated by testgen.sh
module Actual.TestCount where
import TypeLambda
import Data.Typeable
import Expected.TestCount as E
import System.IO

x :: [(L(V(X,X))((V(X,X)),(V(X,X))),(L(V((),()))(L(V((X,X),()))(L(V(X,X,X))(Iff,Int,(Plus,(Iff,Int,Integer,((L(V(X,()))(L(V((),X))(Iff,Integer,Int,(Minus,(V(X,())),(V((),X)))))),(Car,(V((X,X),()))),(V(X,X,X)))),((V((),())),(V((),())),(Cdr,(V((X,X),()))),(V(X,X,X)))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))]
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