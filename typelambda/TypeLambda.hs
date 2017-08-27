module TypeLambda where
import Data.Typeable
import Data.List
import Data.Bool
import qualified Data.Map.Strict as M

data Trans deriving (Typeable)
data Plus  deriving (Typeable)
data Minus deriving (Typeable)
data Max   deriving (Typeable)
data Iff   deriving (Typeable)
data Car   deriving (Typeable)
data Cdr   deriving (Typeable)
data Cons  deriving (Typeable)
data L a b deriving Typeable -- Lambda Param Body
data V a   deriving Typeable -- Val
data X     deriving Typeable

prims :: M.Map String Result
prims = M.fromList [
    ("Trans"  , wrap1 trans), -- TODO: move to type lambda expression
    ("Plus"   , wrap2 plus),
    ("Minus"  , wrap2 minus),
    ("Max"    , wrap2 max'),  -- TODO: move to type lambda expression
    ("Iff"    , wrap3 iff),
    ("Int"    , I 0),
    ("Integer", I 1),
    ("()"     , L []),
    ("Car"    , wrap1 car),
    ("Cdr"    , wrap1 cdr),
    ("Cons"   , wrap2 cons)
  ]

unwrap1 :: Result -> (Result -> Result)
unwrap1 (F f) = f
unwrap1 _ = error "expected function"

-- Used from within lambda eval to make error message more readable
unwrap1' :: Result -> (Result -> Result)
unwrap1' (F f) = f
unwrap1' _ = error "could not apply to arg, as object was not a function"

-- Not used (other than as utility function in tests)
unwrap2 :: Result -> (Result -> Result -> Result)
unwrap2 f = unwrap1 . unwrap1 f

unwrapList :: Result -> [Result]
unwrapList (L xs) = xs
unwrapList _      = error "expected List"

unwrapListInt :: Result -> [Int]
unwrapListInt (L xs) = map unwrapInt xs
unwrapListInt _ = error "expected List"

unwrapListList :: Result -> [[Result]]
unwrapListList = map unwrapList . unwrapList

unwrapInt :: Result -> Int
unwrapInt (I x) = x
unwrapInt _  = error "expected Int"

wrap1 :: (Result -> Result) -> Result
wrap1 f = F f

wrap2 :: (Result -> Result -> Result) -> Result
wrap2 f = F $ \x -> wrap1 (f x)

wrap3 :: (Result -> Result -> Result -> Result) -> Result
wrap3 f = F $ \x -> wrap2 (f x)

wrapListList :: [[Result]] -> Result
wrapListList = L . map L

trans :: Result -> Result
trans = wrapListList . transpose . unwrapListList

car :: Result -> Result
car (L []) = error "called car on empty list"
car (L xs) = head xs
car _ = error "expected List"

cdr :: Result -> Result
cdr (L xs) = L $ tail xs
cdr _ = error "expected List"

cons :: Result -> Result -> Result
cons x (L xs) = L (x:xs)
cons _ _      = error "cons: arg 2 isn't a list"

plus :: Result -> Result -> Result
plus (I a) (I b) = I (a + b)
plus _ _ = error "expected Int Int"

minus :: Result -> Result -> Result
minus (I a) (I b) = I (a - b)
minus _ _ = error "expected Int Int"

max' :: Result -> Result -> Result
max' (I a) (I b) = I (max a b)
max' _ _ = error "expected Int Int"

-- the name "bool" was taken, so calling it "iff" (if and only if)
iff :: Result -> Result -> Result -> Result
iff f g (I  0) = f -- false => return first arg
iff f g (I  _) = g -- true  => return second arg
iff f g (L []) = f
iff f g (L  _) = g
iff _ _ _      = error "arg 3 isn't an Int or List"

-- A lambda expression can either be a primitive value, or a function (which may return another function).
-- Unlike Haskell, the lambda calculus is not statically typed.
-- It allows lists of mixed types (e.g. integers and functions),
-- and functions may consume a variable number of arguments (although only consume one argument at a time).
data Result = I Int | L [Result] | F (Result -> Result)
instance Show Result where       -- Ability to show Result only needed for debugging
  show (I x) = show x
  show (L x) = show x
  show (F x) = "Result -> Result"

eval :: M.Map String Result -> TypeRep -> Result
eval free t =
  case tname of
    "L"         -> F $ l key (ts !! 1) -- function that will take an arg and apply it to the lambda body
    "V"         -> M.findWithDefault (error $ "Key: " ++ key') key' free
    otherwise   -> M.findWithDefault reduced tname free
  where
    c :: TyCon
    ts :: [TypeRep]
    (c, ts) = splitTyConApp t
    tname = tyConName c
    key :: String
    key = show $ ts !! 0
    key' :: String
    key' = show $ t -- "V " ++ key
    l :: String -> TypeRep -> Result -> Result
    l param bod arg = eval (M.insert param arg free) bod
    args :: [Result]
    args = map (eval free) ts
    apply :: Result -> Result -> Result
    apply f g = unwrap1' f $ g
    reduced :: Result
    reduced = foldl apply (head args) (tail args)
