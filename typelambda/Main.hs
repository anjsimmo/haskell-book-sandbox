module Main where
import System.IO
import Histogram

main :: IO ()
main = do
  putStrLn "Please input list of integers in range 0-9: E.g. [0,2,2,1,9]"
  vals <- getLine
  putStrLn $ histogram $ read vals
