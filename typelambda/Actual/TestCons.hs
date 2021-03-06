-- Generated by testgen.sh
module Actual.TestCons where
import TypeLambda
import Data.Typeable
import Expected.TestCons as E
import System.IO

x :: [Cons]
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
