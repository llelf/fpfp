
import Data.List
import Data.Array
import Data.Maybe
import Data.Char
import Data.List.Split
import Control.Arrow

data File = A|B|C|D|E|F|G|H deriving (Enum,Show,Eq,Ord,Ix)
type Dist = Int
data Pos = Pos File Dist deriving (Eq,Ord,Ix)

data Piece = Rock | Knight | Bishop | Queen | King | Pawn deriving (Enum,Eq)

data Move = NormalMove Piece Pos Pos
          | TakingMove Piece Pos Pos
          | Castling Pos Pos
          | Promotion Pos Pos Piece Bool
          | EnPassant Pos Pos

instance Show Pos where
    show (Pos file dist) = [['a'..'h'] !! fromEnum file] ++ show dist

instance Show Piece where
    show piece = showPlace $ Just (White,piece)

showMove' piece pos pos' take rest
    = (if piece /= Pawn then show piece else "")
      ++ (show pos) ++ (if take then "⚔" else "→") ++ (show pos') ++ rest


instance Show Move where
    show (NormalMove piece pos pos') = showMove' piece pos pos' False ""
    show (TakingMove piece pos pos') = showMove' piece pos pos' True ""
    show (Promotion pos pos' piece take) = showMove' Pawn pos pos' take ('=' : show piece)
    show (EnPassant pos pos') = showMove' Pawn pos pos' True "e.p."
    show (Castling _ (Pos A _)) = "O-O-O"
    show (Castling _ (Pos H _)) = "O-O"

data Colour = White | Black deriving (Show,Eq)


-- Board
data Board = Board (Array Pos (Maybe (Colour,Piece))) deriving (Show,Eq)

showPlace (Just (colour,piece)) = [(if colour == White then "♖♘♗♕♔♙" else "♜♞♝♛♚♟") !! fromEnum piece]
showPlace Nothing = "—"

boardBounds = (Pos A 1, Pos H 8)

board :: [(Pos,(Colour,Piece))] -> Board
board pps = Board $ listArray boardBounds (repeat Nothing) // map (second Just) pps

(Board brd) `at` pos = brd ! pos
board `colourAt` pos = do { (colour,_) <- board `at` pos; return colour }

showBoard (Board brd) = unlines . map (("BOARD! "++).concat)
                        . transpose . map reverse . splitEvery 8 . map ((' ':).showPlace) . elems $ brd

boardPieces (Board brd) = [ (pos, x) | (pos, Just x) <- assocs brd ]

positionsBy by = map (\(pos,_) -> pos) . filter (\(_,p) -> by p) . boardPieces
positionsWithColour c = positionsBy (\(c',_) -> c == c')
positionsWithNotColour c board = positionsBy (const True) board \\ positionsWithColour c board

positionOf it = listToMaybe . positionsBy (==it)

isEmpty board pos = isNothing $ board `at` pos
isEnemy board col pos | Just col' <- board `colourAt` pos = col /= col'
                      | otherwise = False

relRank (Pos _ d) White = d
relRank (Pos _ d) Black = 8 - d + 1

enemy White = Black
enemy Black = White

-- FEN

fen :: String -> Board
fen s = Board . listArray boardBounds . concat . transpose . map fill . reverse . splitOn "/" $ brd
    where brd = head . words $ s
          fill = concatMap $ \c -> if isDigit c then replicate (digitToInt c) Nothing
                                   else [charToPiece c]
          charToPiece c = Just (col c, piece $ toLower c)
          col c | isUpper c = White | otherwise = Black
          piece 'p' = Pawn; piece 'r' = Rock
          piece 'n' = Knight; piece 'b' = Bishop
          piece 'q' = Queen; piece 'k' = King


-- Move directions

toDirs :: [(Int->Int, Int->Int)] -> [Pos -> Maybe Pos]
toDirs = map (\(fdo,rdo) -> coordsToPos . (fdo***rdo) . posToCoords)
    where posToCoords (Pos file dist) = (fromEnum file, dist - 1)
          coordsToPos (x,y) | inRange ((0,0),(7,7)) (x,y) = Just $ Pos (toEnum x) (y+1)
                            | otherwise                   = Nothing

afterDirs from = catMaybes . map ($from)


rockDirs = toDirs [(pred,id),(succ,id),(id,pred),(id,succ)]

knightDirs = toDirs [(pred,pred.pred),(succ,pred.pred),(succ.succ,pred),(succ.succ,succ),
                     (succ,succ.succ),(pred,succ.succ),(pred.pred,succ),(pred.pred,pred)]

bishopDirs = toDirs [(pred,pred),(pred,succ),(succ,pred),(succ,succ)]

kingDirs = rockDirs ++ bishopDirs



-- Moves


boardAfterMove board (NormalMove _ pos pos') = boardChange board $ changesForMove board pos pos'
boardAfterMove board (TakingMove _ pos pos') = boardChange board $ changesForMove board pos pos'

boardAfterMove board (Castling king@(Pos kf kr) rock@(Pos rf rr))
    = boardChange board $ changesForMove board king king' ++ changesForMove board rock rock'
      where (kf',rf') = if rf > kf then (G,F) else (C,D)
            king' = Pos kf' kr
            rock' = Pos rf' rr
        

boardAfterMove board (EnPassant pos@(Pos ff fr) pos'@(Pos tf tr))
    = boardChange board ((Pos tf fr, Nothing) : changesForMove board pos pos')

boardAfterMove board (Promotion from to piece _) = boardChange board [(from,Nothing), (to,Just(col,piece))]
    where Just col = board `colourAt` from

boardChange (Board brd) changes = Board $ brd // changes

changesForMove board from to = [(from, Nothing), (to, board `at` from)]


-- NB wrong assumption that en passant et castling is always available

-- Pawn special

moveOrPromote from take pos@(Pos f 8) = [ Promotion from (Pos f 8) p take | p <- [Queen,Knight,Rock,Bishop] ]
moveOrPromote from take pos = [ (if take then TakingMove else NormalMove) Pawn from pos ]

pawnNormMoves board from poses = concatMap (moveOrPromote from False) . filter (isEmpty board) $ poses
pawnTakingMoves board from col poses = concatMap (moveOrPromote from True) . filter (isEnemy board col) $ poses
pawnEnPassantMoves board from@(Pos _ myR) col poses = map (EnPassant from) . filter isOK $ poses
    where
      isOK pos@(Pos targetF targetD) = rank == 4 && isEmpty board pos && isEnemyPawn board col (Pos targetF myR)
          where
            rank = relRank from (enemy col)
            isEnemyPawn board col pos | Just (col',Pawn) <- board `at` pos = col /= col'
                                      | otherwise                          = False

pawnMoves :: Board -> Pos -> [Move]
pawnMoves board from@(Pos f r) = pawnNormMoves board from normPos
                                 ++ pawnTakingMoves board from col takingPos
                                 ++ pawnEnPassantMoves board from col takingPos
    where
      dir = if col == White then succ else pred
      nDir = toDirs [(id,dir)]
      n2Dir = toDirs [(id,dir.dir)]
      tDir = toDirs [(pred,dir), (succ,dir)]
      Just col = board `colourAt` from
      normPos = afterDirs from nDir -- ++ n2Dir from -- | relRank from col == 2 ]
      takingPos = afterDirs from tDir


-- King special

rankSubLine rank f1 f2 = [ Pos f rank | f <- [f1..f2] ]

castling board from@(Pos file dist)
    | file==E, rank==1, emptyR = [ Castling from (Pos H dist) ]
    | file==E, rank==1, emptyL = [ Castling from (Pos A dist) ]
    | otherwise       = []
    where
      rank = relRank from col
      Just col = board `colourAt` from
      emptyR = all (isEmpty board) $ rankSubLine dist F G
      emptyL = all (isEmpty board) $ rankSubLine dist B D



-- Ordinary

possibleMovesDir dir pos0 pos piece board line
    | Just pos' <- dir pos = case board `at` pos' of
                               Nothing       -> goNorm pos'
                               Just (col',_) -> goTake pos' col'
    | otherwise            = []
    where
      Just col = board `colourAt` pos0
      goNorm pos' = adjNormMoves pos' : restNormMoves pos'
      adjNormMoves pos' = NormalMove piece pos0 pos'
      restNormMoves pos' = if line then possibleMovesDir dir pos0 pos' piece board line
                           else []

      goTake pos' col' = [ TakingMove piece pos0 pos' | col /= col' ]

possibleMoves pos piece board line dirs ch specials
    = filter checkSafe $ ordMoves ++ specialMoves
      where
        ordMoves = concat [ possibleMovesDir dir pos pos piece board line | dir <- dirs ]
        specialMoves = concat [ s board pos | s <- specials ]
        checkSafe = if ch then not . isThereCheckAfter col board
                    else const True
        Just col = board `colourAt` pos



moves' Rock pos board ch   = possibleMoves pos Rock board True rockDirs ch []
moves' Knight pos board ch = possibleMoves pos Knight board False knightDirs ch []
moves' Bishop pos board ch = possibleMoves pos Bishop board True bishopDirs ch []
moves' Queen pos board ch  = possibleMoves pos Queen board True kingDirs ch []
moves' King pos board ch   = possibleMoves pos King board False kingDirs ch [castling]
moves' Pawn pos board ch   = possibleMoves pos Pawn board False [] ch [pawnMoves]



movesChecking check pos board
    | Just (_,piece) <- board `at` pos = moves' piece pos board check
    | otherwise                        = []

unsafeMoves = movesChecking False
moves = movesChecking True



-- Main Logic

canAttack :: Board -> Pos -> Pos -> Bool
canAttack board from to = any canTake ms -- it was bad idea really
    where ms = unsafeMoves from board
          canTake (TakingMove _ from' to') | from'==from, to==to'     = True -- this too
          canTake (Promotion from' to' _ True) | from'==from, to==to' = True
          canTake _                                                   = False



isThereCheck colour board = any (\pos -> canAttack board pos kingPos) enemyPos
    where enemyPos = positionsWithNotColour colour board
          Just kingPos = positionOf (colour,King) board

isThereCheckmate colour board = noHopes && isThereCheck colour board
    where
      noHopes = not $ any (\from -> any noCheckAfter $ moves from board) poses
      poses = positionsWithColour colour board
      noCheckAfter = not . isThereCheckAfter colour board


isItAfter what board move = what board' where board' = boardAfterMove board move

isThereCheckAfter colour board move = isItAfter (isThereCheck colour) board move
isThereCheckmateAfter colour board move = isItAfter (isThereCheckmate colour) board move

oneMoveCheckmate board col = filter (isThereCheckmateAfter (enemy col) board) $ wMoves
    where poses = positionsWithColour col board
          wMoves = concatMap (\pos -> moves pos board) $ poses


b1 = fen "N1n5/1kPP4/8/8/4K3/8/Q7/8 w ---- - 0 1"
b2 = fen "2r4r/6pp/3R4/4kpB1/1Pb5/1n4P1/2P3BP/2R3K1 w ---- - 0 1"
b3 = fen "7k/5Ppp/4K3/8/8/8/8/8 w ---- - 0 1"
b4 = fen "r1bqkb1r/pp1npppp/2p2n2/8/3PN3/8/PPP1QPPP/R1B1KBNR w KQkq - 0 1"
b5 = fen "3B4/8/5K1p/8/7k/5P1p/7P/8 w ---- - 0 1"
b6 = fen "7k/5K1p/6P1/8/8/8/8/8 w ---- - 0 1"
b7 = fen "Q3nknR/ppppp1pp/8/8/8/8/PPPPP1PP/4K2R w K--- - 0 1"
b8 = fen "5k2/Q7/7N/8/8/n7/7q/2K5 w ---- - 0 1"
bp = fen "r1bq1r2/pp2n3/4N2k/3pPppP/1b1n2Q1/2N5/PP3PP1/R1B1K2R w KQ-- g6 0 1"
