--{-# OPTIONS_GHC -Wall #-}

module Beta where

import Data.Bool
import qualified Data.Set as S

type Var = String
data Exp = Lit Var | Lambda Var Exp | App Exp Exp | Subs Exp Var Exp deriving (Eq)
instance Show Exp where
  show (Lit v) = showVar v
  show (Lambda v e) = "λ" ++ showVar v ++ "." ++ show e ++ ""
  show (App e1 e2) = bracketsIfNeeded True e1 ++ " " ++ bracketsIfNeeded False e2
  show (Subs e1 v e2) = "[" ++ showVar v ++ ":=" ++ show e2 ++ "] " ++ show e1

showVar :: Var -> String
showVar = id

bracketsIfNeeded :: Bool -> Exp -> String
bracketsIfNeeded isLHS e = case e of
  Lit _ -> show e
  App _ _ -> bool
               ("(" ++ show e ++ ")")
               (show e) -- Application is left associative by default, so no need for brackets
               isLHS
  _ -> "(" ++ show e ++ ")"

-- Reduce 1 step, normal order
eval :: Exp -> Exp
eval (App (Lambda v e1) e2) = eval (Subs e1 v e2) -- Use eval to do force substitution immediately (for our sanity)
eval (App e1 e2) = App (eval e1) e2 -- Could not apply arg, so expand the inside (recursive, but won't branch)
eval (Subs (Lit v1) v e2) = bool (Lit v1) e2 (v1 == v) -- Reached var level, can perform direct substitution!
eval (Subs (App e11 e12) v e2) = App (eval (Subs e11 v e2)) (eval (Subs e12 v e2))
eval s@(Subs (Lambda v1 e1) v e2) = bool
                                      (let v1' = pickNewName (freeVars s) v1
                                           in bool
                                                (Lambda v1' (eval (Subs (eval (Subs e1 v1 (Lit v1'))) v e2))) -- Rename required to prevent binding ambiguity
                                                (Lambda v1 (eval (Subs e1 v e2))) -- No ambiguity if directly substitute
                                                (v1' == v1)
                                      ) -- When substituting inside of a lambda expression, make sure we rename var to avoid conflicts
                                      (Lambda v1 e1) -- Subsitution var same as head var, any vars in lambda body are shadowed therefore won't match
                                      (v1 == v)
eval (Subs e1 v e2) = Subs (eval e1) v e2 -- Perform inner substitution before proceeding any further
eval x = x -- Can only eval applications of a lambda to an expression! Can't reduce.

-- Extract all unbound vars in the expression. (i.e. free vars)
freeVars :: Exp -> S.Set Var
freeVars (Lit v) = S.singleton v
freeVars (App e1 e2) = S.union (freeVars e1) (freeVars e2)
freeVars (Lambda v e) = S.delete v (freeVars e)
freeVars (Subs e1 _ e2) = S.union (freeVars e1) (freeVars e2)

-- Banned vars, old name, new name
pickNewName :: S.Set Var -> Var -> Var
pickNewName banned v = head $ filter (`S.notMember` banned) (altNames v)

-- Lazily generate infinite list of alternative names for var
altNames :: Var -> [Var]
altNames v = map (mnemonic ++) $ "" : map show [(1::Integer)..]
  where mnemonic = take 1 v

-- Recursively reduce to Head Normal Form
reduce :: Exp -> Exp
reduce e = bool
              (reduce e') -- Not done yet
              e -- We're looping. Stop it!
              (e' == e)
            where e' = eval e
-- TODO: Detect longer loops

-- Recursively reduce all terms
reduceAll :: Exp -> Exp
reduceAll e = case e' of
                (App   e1 e2) -> App (reduceAll e1) (reduceAll e2)
                (Lambda v e2) -> Lambda v (reduceAll e2)
                _             -> e'
              where e' = reduce e

-- Test util functions
l :: Char -> Exp -> Exp
l = Lambda . char2str -- convert char to
  where char2str =  (:[])
(.$) :: Exp -> Exp -> Exp
(.$) = App
vx :: Exp
vx = Lit "x"
vy :: Exp
vy = Lit "y"
vf :: Exp
vf = Lit "f"
va :: Exp
va = Lit "a"
vb :: Exp
vb = Lit "b"
ve :: Exp
ve = Lit "e"
vt :: Exp
vt = Lit "t"
vw :: Exp
vw = Lit "w"

-- Test cases
t1 :: Exp
t1 = l 'x' vx .$ vy
t2 :: Exp
t2 = (l 'f' $ l 'a' $ vf .$ va) .$ (l 'x' $ vx .$ vx) .$ vy
t3 :: Exp
t3 = (l 'b' $ l 't' $ l 'e' $ (vb .$ vt) .$ ve) .$ (l 'x' $ l 'y' vx) .$ vx .$ vy
t4 :: Exp
t4 = (l 'x' $ l 'y' $ vy .$ vx) .$ (vy .$ vw)
t5 :: Exp
t5 = (l 'x' $ l 'x' $ vx) .$ vx .$ vy
t6 :: Exp
t6 = (l 'x' $ vx) .$ (l 'x' $ vx) .$ vx

-- Print out test cases (need to manually verify results)
doTest :: IO ()
doTest = putStrLn $ unlines $ map (\x -> show(x) ++ " => " ++ show (reduceAll x) ) $ [t1,t2,t3,t4,t5,t6]
