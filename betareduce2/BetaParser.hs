{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

module BetaParser where
import Beta
import Control.Applicative
import Data.Char (isLetter, isNumber)

type Error = String
type Parser a = String -> Either Error (a, String)
data Token = TLam | TOpenP | TCloseP | TDot | TVar Var | TSpace | TEmpty deriving (Show, Eq)
-- E.g. given a list of tokens, parse the token as an expression, and return the remaining tokens
type GenericParser a b = a -> Either Error (b, a)

showEval :: String -> String
showEval s = case (parseAll s) of
              Left err -> "Parse Error: " ++ err
              Right e  -> show $ reduceAll $ e

showParse :: String -> String
showParse s = case (parseAll s) of
                Left err -> "Parse Error: " ++ err
                Right e  -> show $ e

parseAll :: String -> Either Error Exp
parseAll s = nestApp =<< gs =<< ts
  where
    ts :: Either Error [Token]
    ts = applyUntilDone s parseToken
    gs :: [Token] -> Either String [Exp]
    gs ts' = applyUntilDone ts' parseTerm

parseToken :: Parser Token
parseToken "" = Left "Expected token, but no more input"
parseToken (' ':xs)  = parseToken xs -- Strip leading whitespace
parseToken ('λ':xs)  = Right (TLam, xs)
parseToken ('\\':xs) = Right (TLam, xs) -- For convenience, allow backslash as alias for lambda symbol
parseToken ('(':xs)  = Right (TOpenP, xs)
parseToken (')':xs)  = Right (TCloseP, xs)
parseToken ('.':xs)  = Right (TDot, xs)
parseToken s@(c:_)
  | isLetter c = readVar s
  | otherwise  = Left $ "Unexpected character: " ++ show(c)

-- Variable name must begin with letter (other than λ), optionally followed by a number
readVar :: Parser Token
readVar []     = Left "Variable name empty"
readVar (c:cs) = Right (TVar (c:cs'), ts')
  where
    (cs', ts') = span isNumber cs

-- Parse a single term of expression
parseTerm :: GenericParser [Token] Exp
parseTerm ts@(TLam:_)   = parseLambda ts
parseTerm ts@(TOpenP:_) = parseParens ts
parseTerm (TVar v:ts')  = Right $ (Lit v, ts')
parseTerm (t:_)         = Left $ "Unexpected token: " ++ show t
parseTerm _             = Left $ "Attempted to parse empty expression"

parseLambda :: [Token] -> Either Error (Exp, [Token])
parseLambda (TLam:ts) = liftA2 (\(e, ts') vs' -> (wrapin Lambda e vs', ts')) b vs
  where
    (hts, bts) = break (== TDot) ts
    vs :: Either String [Var]
    vs = parseHeadVars hts
    bts' = drop 1 bts -- Drop '.' -- TODO: Error if not present
    b :: Either Error (Exp, [Token])
    b = parseApp bts'
parseLambda _ = Left "Expected lambda token"

wrapin :: (a -> b -> b) -> b -> [a] -> b
wrapin = foldr

parseHeadVars :: [Token] -> Either Error [Var]
parseHeadVars [] = Right []
parseHeadVars (TSpace:ts) = parseHeadVars ts -- Ignore spaces
parseHeadVars (TVar v : ts) = (showVar v :) <$> parseHeadVars ts
parseHeadVars _ = Left "Unexpected token in lambda head" -- TODO: Tell user which token(s)

-- Similar to parseParens, but parse until end rather than closing bracket
parseApp :: [Token] -> Either Error (Exp, [Token])
parseApp ts = (\(es,ts') -> (,ts') <$> nestApp es) =<< groups' ts -- TODO: Abstract over groups function to reduce code duplication in this module

-- Similar to groups, but parse until any form of boundary (and don't gobble closing brace from higher level)
groups' :: [Token] -> Either String ([Exp], [Token])
groups' ts = applyUntilBoundary TCloseP ts parseTerm

-- Given a list of tokens :: [a], and a parser :: [a] -> Either String (b, [a])
-- that parses an expression and returns the remaining unparsed tokens,
-- apply the parser recursively to the remainder until the entire list of
-- expressions are parsed
applyUntilDone :: [a] -> ([a] -> Either String (b, [a])) -> Either String [b]
applyUntilDone [] _ = Right []
applyUntilDone ts f = case (f ts) of
  Left  err      -> Left err
  Right (e, ts') -> (e:) <$> (applyUntilDone ts' f)

-- Also appears in Data.Graph.Inductive.Query.Monad (but would require installing library)
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

-- Similar to applyUntilDone, but apply until sentinel value (e.g. closing paren)
applyUntil :: (Eq a, Show a) => a -> [a] -> ([a] -> Either String (b, [a])) -> Either String ([b], [a])
applyUntil end [] _ = Left $ "Expecting closing " ++ show end --e.g. missing ')'
applyUntil end ts@(t:ts'') f
  | t == end  = Right ([], ts'') -- Note that closing token NOT included in list of unparsed tokens
  | otherwise = case (f ts) of
      Left  err      -> Left err
      Right (e, ts') -> mapFst (e:) <$> (applyUntil end ts' f)

-- Similar to applyUntil, but stops at boundary, and doesn't gobble stop character
applyUntilBoundary :: (Eq a, Show a) => a -> [a] -> ([a] -> Either String (b, [a])) -> Either String ([b], [a])
applyUntilBoundary _   [] _ = Right ([], []) -- E.g. end of expression
applyUntilBoundary end ts@(t:_) f
  | t == end  = Right ([], ts) -- Note that closing token IS included in list of unparsed tokens
  | otherwise = case (f ts) of
      Left  err      -> Left err
      Right (e, ts') -> mapFst (e:) <$> (applyUntilBoundary end ts' f)

parseParens :: GenericParser [Token] Exp
-- Needed something like a functor, but that allows the applied function
-- to return a context that replaces the old one.
-- Looking up "(a -> f b) -> f a -> f b" on Hoogle shows we need to use a monad.
-- Thanks to Rhys and Shannon for help with how to do this within a tuple.
-- TODO: Use Traversable (but will need to correct order of tuple to
--                        have element of interest on the right)
parseParens (TOpenP:ts) = (\(es,ts') -> (,ts') <$> nestApp es) =<< groups ts
parseParens _           = Left "Expected open paren"

nestApp :: [Exp] -> Either String Exp
nestApp [] = Left "Empty expression"
nestApp (e:es) = foldl (\acc e' -> flip App e' <$> acc) (Right e) es

groups :: [Token] -> Either String ([Exp], [Token])
groups ts = applyUntil TCloseP ts parseTerm

main :: IO ()
main = do
  putStrLn "\nEnter lambda expression to reduce:"
  s <- getLine
  putStrLn $ (showParse s) ++ " => " ++ (showEval s)
  main
