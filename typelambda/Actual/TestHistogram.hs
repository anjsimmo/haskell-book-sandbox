-- Generated by testgen.sh
module Actual.TestHistogram where
import TypeLambda
import Data.Typeable
import Expected.TestHistogram as E
import System.IO

x :: [(L(V((X,X),()))((L(V((X,X),()))(L(V((),(X,X)))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),Cons,(V((),(X,X))),(V((X,X),()))))),((L(V((X,X),()))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),(L(V((X,X),()))(L(V((),(X,X)))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),Cons,(V((),(X,X))),(V((X,X),()))))),(),((L(V(X,X))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),L(V(X,()))(Cons,((V(X,X)),(V(X,())))),())),L(V((),(X,X)))((L(V((X,X),()))(L(V((),(X,X)))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),Cons,(V((),(X,X))),(V((X,X),()))))),(V((),(X,X))),(Cons,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),Integer)),())),(V((X,X),()))))),(Trans,((L(V((X,X),()))((L(V(X,X))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),L(V(X,()))(Cons,((V(X,X)),(V(X,())))),())),((L(V(X,X,X,X))(L(V(X,()))((L(V((X,X),()))(L(V((),(X,X)))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),Cons,(V((),(X,X))),(V((X,X),()))))),((L(V(X,X,X))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X,X))(L(V(X,()))(Iff,(Cons,(V(X,())),((V((),())),(Minus,(V(X,X,X)),Integer),(V(X,())))),(),((L(V(X,()))(L(V((),X))(Iff,Integer,Int,(Minus,(V(X,())),(V((),X)))))),(V(X,X,X)),Int)))))),(V(X,X,X)))),(Minus,(V(X,X,X,X)),(V(X,()))),(Plus,(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),Integer))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),Integer))),(Plus,Integer,Integer))),((L(V(X,X,X))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X,X))(L(V(X,()))(Iff,(Cons,(V(X,())),((V((),())),(Minus,(V(X,X,X)),Integer),(V(X,())))),(),((L(V(X,()))(L(V((),X))(Iff,Integer,Int,(Minus,(V(X,())),(V((),X)))))),(V(X,X,X)),Int)))))),(V(X,X,X)))),(V(X,())),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),Integer)))),(Plus,Integer,Integer)))))),(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),L(V(X,()))(L(V((),X))(Max,(V(X,())),(V((),X)))),Int,(V((X,X),())))),(V((X,X),())))),((L(V((X,X),()))((L(V(X,X))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),L(V(X,()))(Cons,((V(X,X)),(V(X,())))),())),(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V((X,X),()))(L(V(X,X,X))(Iff,Int,(Plus,(Iff,Int,Integer,((L(V(X,()))(L(V((),X))(Iff,Integer,Int,(Minus,(V(X,())),(V((),X)))))),(Car,(V((X,X),()))),(V(X,X,X)))),((V((),())),(Cdr,(V((X,X),()))),(V(X,X,X)))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),())))))))),(V((X,X),()))),(Cons,Int,(Cons,Integer,(Cons,(Plus,Integer,Integer),(Cons,(Plus,(Plus,Integer,Integer),Integer),(Cons,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),(Cons,(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),Integer),(Cons,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,(Plus,Integer,Integer),Integer)),(Cons,(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,(Plus,Integer,Integer),Integer)),Integer),(Cons,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer))),(Cons,(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer))),Integer),())))))))))))),(V((X,X),())))))),((L(V((X,X),()))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),(L(V((X,X),()))(L(V((),(X,X)))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),Cons,(V((),(X,X))),(V((X,X),()))))),(),((L(V(X,X))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),L(V(X,()))(Cons,((V(X,X)),(V(X,())))),())),L(V((),(X,X)))((L(V((X,X),()))(L(V((),(X,X)))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),Cons,(V((),(X,X))),(V((X,X),()))))),(V((),(X,X))),(Cons,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),Integer)),())),(V((X,X),()))))),(Cons,(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X,X))(L(V(X,()))(Iff,(Cons,(V(X,())),((V((),())),(Minus,(V(X,X,X)),Integer),(V(X,())))),(),((L(V(X,()))(L(V((),X))(Iff,Integer,Int,(Minus,(V(X,())),(V((),X)))))),(V(X,X,X)),Int)))))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),Integer)),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),Integer))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),Integer)))),Integer)),(Cons,((L(V(X,X))(((L(V(X,X))((L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))),(L(V(X,()))((V(X,X)),((V(X,())),(V(X,()))))))),(L(V((),()))(L(V(X,X))(L(V((),(),X))(L(V((X,X),()))(Iff,(V((),(),X)),((V(X,X)),(Car,(V((X,X),()))),((V((),())),(V(X,X)),(V((),(),X)),(Cdr,(V((X,X),()))))),((L(V(X,()))(Iff,Int,Integer,(V(X,())))),(V((X,X),()))))))))),L(V(X,()))(Cons,((V(X,X)),(V(X,())))),())),(Plus,(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),Integer)))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer))))),(Cons,Int,(Cons,Integer,(Cons,(Plus,Integer,Integer),(Cons,(Plus,(Plus,Integer,Integer),Integer),(Cons,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),(Cons,(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer)),Integer),(Cons,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,(Plus,Integer,Integer),Integer)),(Cons,(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,(Plus,Integer,Integer),Integer)),Integer),(Cons,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer))),(Cons,(Plus,((L(V(X,()))(Plus,(V(X,())),(V(X,())))),((L(V(X,()))(Plus,(V(X,())),(V(X,())))),(Plus,Integer,Integer))),Integer),()))))))))))),())))))]
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
