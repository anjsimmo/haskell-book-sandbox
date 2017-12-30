{-# OPTIONS_GHC -Wall #-}

-- Framework that declares interfaces for games and AI
module Framework where

class GameBoard b where
  turn :: b -> Turn
  judge :: b -> Maybe EndGame
  validMoves :: b -> [b]
  skipTurn :: b -> b

data Turn = AI | Human deriving (Eq, Show)
toggleTurn :: Turn -> Turn
toggleTurn AI = Human
toggleTurn Human = AI

data EndGame = L | D | W deriving (Eq, Ord) -- Ordered by utility to AI.
instance Show EndGame where
  show L = "Human Wins"
  show D = "Draw"
  show W = "AI Wins"

type Pos = (Size, Size)
type Size = Int
type Depth = Int
type Intelligence = Depth -- AI strength is tree depth to search

endGameUtil :: EndGame -> Float
endGameUtil L = 0.0
endGameUtil D = 0.5
endGameUtil W = 1.0

-- Rose tree
data Tree a = Tree { root :: a
                   , children :: [Tree a]
                   } deriving (Show)

-- lazily generate game tree
genGameTree :: (GameBoard b) => b -> Tree b
genGameTree gb = Tree gb (map genGameTree (validMoves gb))

trimTree :: Depth -> Tree a -> Tree a
trimTree 0 t = Tree (root t) []
trimTree n t = Tree (root t) (map (trimTree (n-1)) (children t))

type Util = Float

-- e.g. Gameboard labelled with known util
type KnownUtil b = (b, Util)
util :: KnownUtil b -> Util
util (_, u) = u
node :: KnownUtil b -> b
node (b, _) = b

-- Options are usually boards, in which case (o->b) can be the id function,
-- but we use type signature to statically check that ai will select a valid option
-- rather than an arbitrary board configuration. Nothing => Surrender/Skip
type Ai o b = [o] -> (o -> b) -> Maybe o
type ComposeAI o b = (b -> Util) -> Intelligence -> Ai o b
