{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

module GameBoardChess where
import Framework
import Data.Bool
import Control.Monad
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Helpers
import Data.List (intersperse)

type Moved = Bool
type DoubleStep = Bool

-- King, Queen, Rook, Bishop, kNight, Pawn
data Piece = K | Q | R | B | N | P deriving (Show, Ord, Eq)

data Color = White | Black deriving (Show, Ord, Eq)
toggleSide :: Color -> Color
toggleSide White = Black
toggleSide Black = White

data Token = T {tokenColor :: Color, tokenPiece :: Piece, tokenPos :: Pos} deriving (Show, Ord, Eq)

rows :: Size
rows = 8

cols :: Size
cols = 8

type Dist = Size
type Offset = (Dist, Dist)
type Dims = (Size, Size)
type Cell = Pos
type Label = String -- Used for tagging move names

-- Used for purpose of tracking if castling legal
data CastleStatus = AnyCatle | KSideCastle | QSideCastle | NoCastle deriving (Ord, Eq)
instance Show CastleStatus where
  show AnyCatle    = "King Unmoved, Neither Rook Moved"
  show KSideCastle = "King Unmoved, King Side Rook Unmoved"
  show QSideCastle = "King Unmoved, Queen Side Rook Unmoved"
  show NoCastle    = "King/Rooks Moved"

-- Dive Pos represents the square skipped over (not moved to) by a pawn move 2 forward
-- Used for purpose of tracking if En Passant attack by opposition next turn is legal
data EnPassantStatus = Dive Token Pos | NoDive deriving (Ord, Eq)
instance Show EnPassantStatus where
  show NoDive = "N/A"
  show (Dive (T clr _ _) p) = show (clr) ++ " Pawn Passed Over " ++ showP p

data HDir = KSide | QSide deriving (Show, Ord, Eq)

data ChessBoard = ChessBoard
  { aiColor :: Color -- From AI perspective
  , colorTurn :: Color
  , whiteTokens :: S.Set Token
  , blackTokens :: S.Set Token
  , cellIndex :: M.Map Pos Token
  , whiteCastleStatus :: CastleStatus
  , blackCastleStatus :: CastleStatus
  , whiteEnPassantStatus :: EnPassantStatus
  , blackEnPassantStatus :: EnPassantStatus
  , agreedOutcome :: Maybe EndGame -- Allows ai to surrender if it refuses
                                   -- to select any of the provided options
  }
  deriving (Ord, Eq)
  -- TODO: Allow free conversion between black/white perspectives to reduce repeated code

enPassantStatus :: ChessBoard -> EnPassantStatus
enPassantStatus b = case colorTurn b of
  White -> blackEnPassantStatus b --- During white's turn, only care about black's movements
  Black -> whiteEnPassantStatus b

instance GameBoard ChessBoard where
  turn b = bool Human AI (aiColor b == colorTurn b)
  judge b = bool (judge' b) (agreedOutcome b) (isJust $ agreedOutcome b)
  validMoves = map snd . allMoves' -- just moves, not labels
  skipTurn b = b {
                   colorTurn = (colorTurn b), -- TODO: Is switching turns still necessary once game over?
                   -- Admit defeat (unless someone has already surrendered)
                   agreedOutcome = bool (Just $ surrender b) (agreedOutcome b) (isJust $ agreedOutcome b)
                 }

instance Show ChessBoard where
  show cb = showBoard cb ++ "\n" ++ showStatus cb ++ "\n\nMoves: " ++ showMoves cb
    where
      showBoard :: ChessBoard -> String
      showBoard b = prettyBoard rows cols (\p -> M.lookup p (cellIndex b)) " ·" prettyToken
      showStatus :: ChessBoard -> String
      showStatus b = "Turn: " ++ show (colorTurn b)
                     ++ "\nWhite Status: " ++ show (whiteCastleStatus b)
                     ++ "\nBlack Status: " ++ show (blackCastleStatus b)
                     ++ "\nEn Passant Status: " ++ show (enPassantStatus b)
      showMoves :: ChessBoard -> String
      showMoves b = concat $ intersperse " " $ map fst (allMoves' b)

-- https://en.wikipedia.org/wiki/Chess_symbols_in_Unicode
prettyToken :: Token -> String
prettyToken (T White K _) = " ♔"
prettyToken (T White Q _) = " ♕"
prettyToken (T White R _) = " ♖"
prettyToken (T White B _) = " ♗"
prettyToken (T White N _) = " ♘"
prettyToken (T White P _) = " ♙"
prettyToken (T Black K _) = " ♚"
prettyToken (T Black Q _) = " ♛"
prettyToken (T Black R _) = " ♜"
prettyToken (T Black B _) = " ♝"
prettyToken (T Black N _) = " ♞"
prettyToken (T Black P _) = " ♟"

tokens :: Color -> ChessBoard -> S.Set Token
tokens White = whiteTokens
tokens Black = blackTokens

castleStatus :: Color -> ChessBoard -> CastleStatus
castleStatus White = whiteCastleStatus
castleStatus Black = blackCastleStatus

getCell :: ChessBoard -> Pos -> Maybe Token
getCell b p = M.lookup p (cellIndex b)

genRange :: Dims -> Cell -> Offset -> [Cell]
genRange dims s o = takeWhile (inBoundary dims) (slide s o)

inBoundary :: Dims -> Cell -> Bool
inBoundary (sX, sY) (x, y) = (x >= 0) && (y >= 0) && (x < sX) && (y < sY)

-- Don't include start cell as option
genRange' :: Dims -> Cell -> Offset -> [Cell]
genRange' dims s = (drop 1) . genRange dims s

slide :: Cell -> Offset -> [Cell]
slide c o = c : slide (vectorAdd c o) o

vectorAdd :: Cell -> Offset -> Cell
vectorAdd (x, y) (dx, dy) = (x + dx, y + dy)

hasPiece :: ChessBoard -> Pos -> Bool
hasPiece b p = isJust (getCell b p)

isFree :: ChessBoard -> Pos -> Bool
isFree b p = not $ hasPiece b p

-- To be valid, attack must be within the bounds of board, and attack the opposition color.
-- There is no need to explicitly check the bounds of the board, under the assumption
-- that the opposition pieces are always within the bounds of the board.
validAttack :: ChessBoard -> Color -> Pos -> Bool
validAttack b c p = colorAtCell p == Just (toggleSide c) -- can only attack opposition color
  where
    colorAtCell :: Pos -> Maybe Color -- color piece at cell
    colorAtCell p' = (tokenColor <$> getCell b p')

movesAttack :: ChessBoard -> Color -> [Pos] -> ([Pos], Maybe Pos)
movesAttack b c ps = (moves, attack')
  where
    moves :: [Pos]
    attacks :: [Pos]
    (moves, attacks) = break (hasPiece b) ps
    attack :: Maybe Pos
    attack = listToMaybe attacks -- can only reach first item in line of sight, not those behind it
    attack' :: Maybe Pos
    attack' = mfilter (validAttack b c) attack -- can only attack opposition color

-- more efficient to generate moves and attacks together due to slide search
allMoveAttacks :: ChessBoard -> Color -> [[Pos]] -> ([Pos], [Pos])
allMoveAttacks b c sweeps = (moves, attacks)
  where
    movesAttacks :: [([Pos], Maybe Pos)]
    movesAttacks = map (movesAttack b c) sweeps
    moves :: [Pos]
    moves = concat $ map fst movesAttacks -- staying still doesn't count as an option
    attacks :: [Pos]
    attacks = concat $ map (maybeToList . snd) movesAttacks -- valid attacks in any direction

diag :: [Offset]
diag = [(-1,1), (-1,-1), (1,-1), (1,1)]
orthog :: [Offset]
orthog = [(-1,0),(0,-1),(1,0),(0,1)]
star :: [Offset]
star = diag ++ orthog
knightPat :: [Offset]
knightPat = [(1,2),(2,1),(-1,2),(-2,1),(1,-2),(2,-1),(-1,-2),(-2,-1)]

genSweeps :: Pos -> [Offset] -> [[Pos]]
genSweeps p os = map (genRange' (rows, cols) p) os

genLeaps :: Pos -> [Offset] -> [[Pos]]
genLeaps p os = map singletonList $ filter (inBoundary (rows, cols)) $ map (vectorAdd p) os

singletonList :: a -> [a]
singletonList = pure

dirVect :: Color -> Offset
dirVect White = (-1,0) -- 0,0 => Top left, but white at bottom of board. TODO: Revise coordinate system
dirVect Black = ( 1,0)

genPawnMoveAttacks :: ChessBoard -> Color -> Pos -> ([Pos], [Pos])
genPawnMoveAttacks b clr p = (moves, attacks)
  where
    step1 = dirVect clr
    mv1 = vectorAdd p step1
    mv2 = vectorAdd mv1 step1
    isFree1 = isFree b mv1
    isFree2 = isFree b mv2
    moves = bool
      []
      (bool
        [mv1]
        [mv1, mv2]
        (isPawnRow clr p && isFree2)
      )
      isFree1
    attacks = filter (validAttack b clr) (genPawnAttacks clr p)

-- Squares that a pawn could attack.
-- Does not including special moves like en passant.
-- Not filtered to check if piece present they could actually attack (i.e. includes attacks on empty squares / own team).
-- Not filtered to check if square is on the board (e.g. could result in negative coordinates)
genPawnAttacks :: Color -> Pos -> [Pos]
genPawnAttacks clr p = [diagL, diagR] -- TODO: filter to just attacks within bounds of board
  where
    step1 = dirVect clr
    diagL = vectorAdd p (vectorAdd (0,-1) step1)
    diagR = vectorAdd p (vectorAdd (0, 1) step1)

allPieceMoveAttacks :: ChessBoard -> Token -> ([Pos], [Pos])
allPieceMoveAttacks b (T s K p) = allMoveAttacks b s (genLeaps  p star)
allPieceMoveAttacks b (T s Q p) = allMoveAttacks b s (genSweeps p star)
allPieceMoveAttacks b (T s R p) = allMoveAttacks b s (genSweeps p orthog)
allPieceMoveAttacks b (T s B p) = allMoveAttacks b s (genSweeps p diag)
allPieceMoveAttacks b (T s N p) = allMoveAttacks b s (genLeaps  p knightPat)
allPieceMoveAttacks b (T s P p) = genPawnMoveAttacks b s p

-- Include both moves and attacks
allPieceMoves :: ChessBoard -> Token -> [Pos]
allPieceMoves b pc = merge2 $ allPieceMoveAttacks b pc
  where
    merge2 :: ([a], [a]) -> [a]
    merge2 (x, y) = x ++ y

getRow :: Pos -> Size
getRow = fst

getCol :: Pos -> Size
getCol = snd

-- useful for determining location pawn skips over (i.e. location of en passant vulnerability) when the pawn dives forward 2 cells
midPos :: Pos -> Pos -> Pos
midPos (x1, y1) (x2, y2) = (((x1 + x2) `div` 2), ((y1 + y2) `div` 2))

-- Includes attacks on empty squares. Doesn't include special attacks like en passant
allPieceAttacks :: ChessBoard -> Token -> [Pos]
allPieceAttacks _ (T clr P p) = genPawnAttacks clr p -- Pawns are special case. Just because a pawn can move to a square doesn't mean they can attack it
allPieceAttacks b pc = allPieceMoves b pc            -- Pieces other than pawns can attack any square they can move to

-- Chess "algebraic notation"
-- https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
showP :: Pos -> String
showP (ri,ci) = showCol ci : showRow ri : []
  where
    showRow r
      | (r >= 0) && (r < rows) = ['1'..] !! (rows - 1 - r) -- Intenrally, we use matrix row,col with (0,0) in top left. But chess algebraic notation uses col,row with "a1" in bottom left.
      | otherwise              = '?' -- Row out of bounds (not part of official notation)
    showCol c
      | (c >= 0) && (c < cols) = ['a'..] !! c
      | otherwise              = '?' -- Col out of bounds (not part of official notation)

-- Show Piece
showPc :: Piece -> String
showPc piece = bool (show piece) "" (piece == P) -- algebraic chess notation assumes Pawn, so no need to display it

-- En Passant pawn capture. enemy :: Token, position moved (not same as enemy location) :: Pos, original attacking piece at old pos :: Token
-- Assumption: en passant attack (attack 3rd row from back) mutually exclusive of pawn promotion (far row) and pawn dive (pawn row). (Assumption holds for ordinary chess game)
enPassantBoardMove :: Token -> Pos -> Token -> ChessBoard -> (Label, ChessBoard)
enPassantBoardMove deadT newP t@(T _ piece _) b = (label, multiMove [t, deadT] [newT] b)
  where
    oldPos :: Pos
    oldPos = tokenPos t
    newT = t {tokenPos = newP}
    label = showPc piece ++ showP oldPos ++ "x" ++ showP newP ++ "e.p."

-- Move a piece forward, attacking enemy (if present), setting en passant dive status (if relevant), possibly leading to promotion (if pawn)
-- Assumption: Promotion (back row only) and Dive (pawn row only) are mutually exclusive. (Assumption holds for ordinary chess game)
ordinaryBoardMove :: Token -> Pos -> ChessBoard -> [(Label, ChessBoard)]
ordinaryBoardMove pc@(T clr piece _) newPos b
  | isPromotion = map (\x -> ordinaryBoardMoveProm' x) [Q, R, B, N]
  | isDive      = singletonList $ (\b' -> b' { whiteEnPassantStatus = updateEnPassant White, blackEnPassantStatus = updateEnPassant Black }) <$> ordinaryBoardMove'
  | otherwise   = singletonList $ ordinaryBoardMove'
  where
    oldPos :: Pos
    oldPos = tokenPos pc
    attackedPiece :: Maybe Token
    attackedPiece = getCell b newPos
    isAttack :: Bool
    isAttack = isJust attackedPiece
    isPromotion :: Bool
    isPromotion = (piece == P) && isFarRow
    isFarRow :: Bool
    isFarRow = getRow newPos == getFarRow clr
    label :: String
    label = showPc piece ++ showP oldPos ++ (bool "" "x" isAttack) ++ showP newPos
    deadTs = [pc] ++ maybeToList attackedPiece -- old piece, and any piece it kills must be removed from index
    movedPc = pc {tokenPos = newPos}
    updateEnPassant :: Color -> EnPassantStatus
    updateEnPassant statusClr = bool
      NoDive
      (Dive movedPc (midPos oldPos newPos))
      (statusClr == clr)
    isDive :: Bool
    isDive = (piece == P) && abs (getRow newPos - getRow oldPos) > 1 -- Only makes sense for pawns, meaningles for other pieces
    ordinaryBoardMoveProm' :: Piece -> (Label, ChessBoard)
    ordinaryBoardMoveProm' promP = (label', multiMove deadTs [promotedPc] b)
      where
        label' :: String
        label' = label ++ show promP
        promotedPc :: Token
        promotedPc = movedPc {tokenPiece = promP}
    ordinaryBoardMove' :: (Label, ChessBoard)
    ordinaryBoardMove' = (label, multiMove deadTs [movedPc] b)

-- Perform castle, updating castle status.
-- deadTs is all pieces that have *moved* or died (despite name, not all deadTs are dead)
castleBoardMove :: [Token] -> [Token] -> ChessBoard -> (Label, ChessBoard)
castleBoardMove deadTs newTs b = (label, multiMove deadTs newTs b)
  where
    label = bool "0-0" "0-0-0" isQueenSide
    isQueenSide :: Bool
    isQueenSide = (getCastleInit (colorTurn b) QSide) `elem` deadTs -- has queen side castle moved (if so, this must be a queenside castle)

-- Any moved or taken pieces must explicitly be passed to the first argument (in order to ensure proper index updates if a piece steps on top of another)
-- Assumption: not a pawn dive / promotion (i.e. assume we don't need to set the dive flag, or deal with pawn promotion)
multiMove :: [Token] -> [Token] -> ChessBoard -> ChessBoard
multiMove deadTs newTs b = b { colorTurn = toggleSide (colorTurn b)
                             , whiteTokens = whiteTokens'
                             , blackTokens = blackTokens'
                             , cellIndex = cellIndex'
                             , whiteCastleStatus = updateCastleStatus White deadTs (whiteCastleStatus b) -- TODO: For efficiency, only update color of our own side
                             , blackCastleStatus = updateCastleStatus Black deadTs (blackCastleStatus b)
                             , whiteEnPassantStatus = NoDive
                             , blackEnPassantStatus = NoDive
                             }
  where
    blackTokens' :: S.Set Token
    blackTokens' = foldr (updateAdd Black) (foldr (updateDel Black) (blackTokens b) deadTs) newTs
    whiteTokens' :: S.Set Token
    whiteTokens' = foldr (updateAdd White) (foldr (updateDel White) (whiteTokens b) deadTs) newTs
    updateDel :: Color -> Token -> S.Set Token -> S.Set Token
    updateDel c deadT ts = bool
      ts                 -- only need to search set corresponding to token's color
      (S.delete deadT ts)
      (tokenColor deadT == c)
    updateAdd :: Color -> Token -> S.Set Token -> S.Set Token
    updateAdd c newT ts = bool
      ts                 -- only need to search set corresponding to token's color
      (S.insert newT ts)
      (tokenColor newT == c)
    updateDelCellIndex :: Token -> M.Map Pos Token -> M.Map Pos Token
    updateDelCellIndex deadT m = M.delete (tokenPos deadT) m
    updateAddCellIndex :: Token -> M.Map Pos Token -> M.Map Pos Token
    updateAddCellIndex newT m = M.insert (tokenPos newT) newT m
    cellIndex' :: M.Map Pos Token
    cellIndex' = foldr updateAddCellIndex (foldr updateDelCellIndex (cellIndex b) deadTs) newTs

getCastleInit :: Color -> HDir -> Token
getCastleInit clr s = case s of
    QSide -> T clr R (r,0)
    KSide -> T clr R (r,7)
  where r = getHomeRow clr

getKnightInit :: Color -> HDir -> Token
getKnightInit clr s = case s of
    QSide -> T clr N (r,1)
    KSide -> T clr N (r,6)
  where r = getHomeRow clr

getBishopInit :: Color -> HDir -> Token
getBishopInit clr s = case s of
    QSide -> T clr B (r,2)
    KSide -> T clr B (r,5)
  where r = getHomeRow clr

getQueenInit :: Color -> Token
getQueenInit clr = T clr Q (r,3)
  where r = getHomeRow clr

getKingInit :: Color -> Token
getKingInit clr = T clr K (r,4)
  where r = getHomeRow clr

getPawnRow :: Color -> Size
getPawnRow Black = 1           -- 0 indexed. home row (king, queen, etc.) => 0, pawn row => 1
getPawnRow White = rows - 2    -- last row => rows-1, seconds last => rows-2

-- Convenience function
getCastleInitPos :: Color -> HDir -> Pos
getCastleInitPos clr s = tokenPos $ getCastleInit clr s

-- Convenience function
getKingInitPos :: Color -> Pos
getKingInitPos clr = tokenPos $ getKingInit clr

-- Convenience function
isPawnRow :: Color -> Pos -> Bool
isPawnRow clr (r, _) = r == getPawnRow clr

-- given list of tokens that moved, determine new castle status for team
updateCastleStatus :: Color -> [Token] -> CastleStatus -> CastleStatus
updateCastleStatus clr ts cstatus = case cstatus of
  NoCastle             -> NoCastle
  KSideCastle
    | kingMoved        -> NoCastle
    | ksideCastleMoved -> NoCastle
    | otherwise        -> KSideCastle
  QSideCastle
    | kingMoved        -> NoCastle
    | qsideCastleMoved -> NoCastle
    | otherwise        -> QSideCastle
  AnyCatle
    | kingMoved        -> NoCastle
    | ksideCastleMoved -> QSideCastle -- Under assumption that only a single castle will move at a time
    | qsideCastleMoved -> KSideCastle
    | otherwise        -> AnyCatle
  where
    kingMoved        = any (== getKingInit clr)         ts
    ksideCastleMoved = any (== getCastleInit clr KSide) ts
    qsideCastleMoved = any (== getCastleInit clr QSide) ts

enPassant :: ChessBoard -> Color -> [(Label, ChessBoard)]
enPassant b clr = attacks'
  where
    attacks :: [(Token, Pos, Token)]
    attacks = bool
      (enPassant' b clr (whiteEnPassantStatus b)) -- If we are black, we can only attack white
      (enPassant' b clr (blackEnPassantStatus b))
      (clr == White)
    attacks' :: [(Label, ChessBoard)]
    attacks' = map (\(deadT, newP, t) -> enPassantBoardMove deadT newP t b) attacks

vFlipVect :: Offset -> Offset
vFlipVect (dr,dc) = (-dr,dc)

-- return type: threatend token, at pos, by attacker
enPassant' :: ChessBoard -> Color -> EnPassantStatus -> [(Token, Pos, Token)]
enPassant' b clr epst = attackers
  where
    attackers :: [(Token, Pos, Token)]
    attackers = case epst of
      Dive t p -> map (t,p,) $ filter isValidAttacker $ catMaybes $ [fromL' p, fromR' p]
      NoDive -> []
    fromL :: Pos -> Pos
    fromL p' = vectorAdd p' (vectorAdd (vFlipVect (dirVect clr)) (0,-1))
    fromR :: Pos -> Pos
    fromR p' = vectorAdd p' (vectorAdd (vFlipVect (dirVect clr)) (0, 1))
    fromL' :: Pos -> Maybe Token
    fromL' p' = getCell b $ fromL p'
    fromR' :: Pos -> Maybe Token
    fromR' p' = getCell b $ fromR p'
    isValidAttacker :: Token -> Bool
    isValidAttacker pc = (tokenPiece pc == P) && (tokenColor pc == clr) -- Only pawns on our side can make en passant attack

-- For efficiency, it is best to check multiple cells at once.
-- True if any of the cells can be attacked by our side
isAttackable :: ChessBoard -> [Cell] -> Bool
isAttackable b = any (`elem` attacks)
  where
    attacks :: [Pos]
    attacks = concat $ map (allPieceAttacks b) $ S.toList (tokens clr b)
    clr :: Color
    clr = colorTurn b

-- For efficiency, it is best to check multiple cells at once.
-- True if any of the cells are under attack from opposition
isUnderAttack :: ChessBoard -> [Cell] -> Bool
isUnderAttack b = isAttackable b'
  where
    b' = b { colorTurn = theirClr } -- gen attacks from opponent's perspective
    theirClr = toggleSide (colorTurn b)

getHomeRow :: Color -> Size
getHomeRow clr = bool 0 (rows - 1) (clr == White)

getFarRow :: Color -> Size
getFarRow = getHomeRow . toggleSide

queensideCastle :: Color -> ChessBoard -> Maybe (Label, ChessBoard)
queensideCastle clr = castle clr QSide

kingsideCastle :: Color -> ChessBoard -> Maybe (Label, ChessBoard)
kingsideCastle clr = castle clr KSide

castle :: Color -> HDir -> ChessBoard -> Maybe (Label, ChessBoard)
castle clr hdir b = bool
                           Nothing
                           (Just
                             (castleBoardMove
                               [rook, king]
                               [
                                 rook {tokenPos = bool (r,3) (r,5) (hdir == KSide)},
                                 king {tokenPos = bool (r,2) (r,6) (hdir == KSide)}
                               ]
                               b
                             )
                           )
                           legal
  where
    allowed = castleStatus clr b `elem` [AnyCatle, (bool QSideCastle KSideCastle (hdir == KSide))]
    r :: Size
    r = getHomeRow clr
    path :: [Pos]
    path = bool [(r,1), (r,2), (r,3)] [(r,6), (r,5)] (hdir == KSide)
    clearPath :: Bool
    clearPath = all (isFree b) path
    safePath :: [Pos] -- squares that must not be under attack when castling
    safePath = bool [(r,4), (r,3), (r,2)] [(r,4), (r,5), (r,6)] (hdir == KSide) -- note that this includes King's current square (as King must not be in check)
    safe :: Bool
    safe = not $ isUnderAttack b safePath
    legal :: Bool
    -- For efficinency, order of castle conditions is important.
    -- Quickest tests on left to take advantage of short circuit evaluation.
    legal = allowed && clearPath && safe
    rook = getCastleInit clr hdir -- Under assumption that Rook hasn't moved
    king = getKingInit clr        -- Under assumption that King hasn't moved

specialMoves :: ChessBoard -> [(Label, ChessBoard)]
specialMoves b = enPassant b clr ++ catMaybes [
    queensideCastle clr b, -- towards Rook on Queen's side
    kingsideCastle clr b   -- towards Rook on King's side
  ]
  where
    clr = colorTurn b

allMoves :: ChessBoard -> [(Label, ChessBoard)]
allMoves b = allOrdinaryMoves ++ allSpecialMoves
   where
     clr :: Color
     clr = colorTurn b
     mvs :: [(Token, [Pos])]
     mvs = map (\t -> (t, allPieceMoves b t)) $ S.toList (tokens clr b)
     allOrdinaryMoves :: [(Label, ChessBoard)]
     allOrdinaryMoves = concat $ map (\(t, ps) -> concat $ map (\p -> ordinaryBoardMove t p b) ps) mvs
     allSpecialMoves :: [(Label, ChessBoard)]
     allSpecialMoves = specialMoves b

getKingPos :: Color -> ChessBoard -> [Cell]
getKingPos clr b' = map tokenPos $ filter (\t -> tokenPiece t == K) $ S.toList (tokens clr b')

allMoves' :: ChessBoard -> [(Label, ChessBoard)]
allMoves' b = filter (\(_, b') -> not $ isAttackable b' (ourKPos b')) (allMoves b) -- b' is from opponent's perspective, so need to make sure they can't *attack* our king
  where
    ourKPos :: ChessBoard -> [Cell]
    ourKPos b' = getKingPos ourClr b' -- Board is from opponent's perspective, but get king of our *own* color
    ourClr = colorTurn b

surrender :: ChessBoard -> EndGame
surrender b = bool W L (aiClr == turnClr) -- Current color loses. W/L is from AI perspective.
  where
    aiClr :: Color
    aiClr = aiColor b
    turnClr :: Color
    turnClr = colorTurn b

judge' :: ChessBoard -> Maybe EndGame
judge' b = bool
             Nothing -- If at least one valid move, then neither stalemate nor checkmate. Play on.
             (bool
               (Just D) -- Stale Mate
               (Just (surrender b)) -- Under attack and can't move to a legal (check free) position.
               (isUnderAttack b (getKingPos turnClr b))
             )
             (null (allMoves' b))
  where
    turnClr :: Color
    turnClr = colorTurn b

emptyChess :: ChessBoard
emptyChess = ChessBoard
                   { aiColor = Black
                   , colorTurn = White
                   , whiteTokens = S.fromList w
                   , blackTokens = S.fromList b
                   , cellIndex = buildIndex (w ++ b)
                   , whiteCastleStatus = AnyCatle
                   , blackCastleStatus = AnyCatle
                   , whiteEnPassantStatus = NoDive
                   , blackEnPassantStatus = NoDive
                   , agreedOutcome = Nothing
                   }
  where
    fs :: [Color -> HDir -> Token]
    fs = [getCastleInit, getKnightInit, getBishopInit]
    genRNBs :: Color -> [Token]
    genRNBs clr = fs <*> [clr] <*> [KSide, QSide]
    genPs :: Color -> [Token]
    genPs clr = [T clr P (getPawnRow clr, clmn) | clmn <- [0..7]]
    genSide clr = genPs clr ++ genRNBs clr ++ [getQueenInit clr, getKingInit clr]
    w :: [Token]
    w = genSide White
    b :: [Token]
    b = genSide Black
    buildIndex :: [Token] -> M.Map Cell Token
    buildIndex ts = M.fromList $ map (\t -> (tokenPos t, t)) ts

-- TODO: Add this to game board class?
safePlaceSimpleChess :: Label -> ChessBoard -> Maybe ChessBoard
safePlaceSimpleChess l b = M.lookup l (M.fromList (allMoves' b))
