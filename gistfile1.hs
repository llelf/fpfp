
import Data.List
import Data.Array
import Data.Maybe
import Control.Arrow

data File = A|B|C|D|E|F|G|H deriving (Enum,Show,Eq,Ord,Ix)
type Dist = Int
data Pos = Pos File Dist deriving (Eq,Ord,Ix)

data Piece = Rock | Knight | Bishop | Queen | King | Pawn deriving (Enum,Eq)

data Move = Move Piece Pos Pos Bool

instance Show Pos where
    show (Pos file rank) = [['a'..'h'] !! fromEnum file] ++ show rank

instance Show Piece where
    show piece = ["♖♘♗♕♔♙" !! fromEnum piece]

instance Show Move where
    show (Move piece _ pos taking) = show piece ++ (if taking then "x" else "") ++ show pos

data Colour = White | Black deriving (Show,Eq)


-- Board
data Board = Board (Array Pos (Maybe (Colour,Piece))) deriving Show

board :: [(Pos,(Colour,Piece))] -> Board
board pps = Board $ listArray (Pos A 1, Pos H 8) (repeat Nothing) // map (second Just) pps

(Board brd) `at` pos = brd ! pos

boardPieces (Board brd) = [ (pos, x) | (pos, Just x) <- assocs brd ]

positionsBy by = map (\(pos,_) -> pos) . filter (\(_,p) -> by p) . boardPieces
positionsWithColour c = positionsBy (\(c',_) -> c == c')
positionsWithNotColour c board = positionsBy (const True) board \\ positionsWithColour c board

positionOf it = listToMaybe . positionsBy (==it)

posToCoords (Pos file rank) = (fromEnum file, rank - 1)
coordsToPos (x,y) | inRange ((0,0),(7,7)) (x,y) = Just $ Pos (toEnum x) (y+1)
                  | otherwise                   = Nothing


-- Move directions

toDirs :: [(Int->Int, Int->Int)] -> [Pos -> Maybe Pos]
toDirs = map (\(fdo,rdo) -> coordsToPos . (fdo***rdo) . posToCoords)

rockDirs = toDirs [(pred,id),(succ,id),(id,pred),(id,succ)]

knightDirs = toDirs [(pred,pred.pred),(succ,pred.pred),(succ.succ,pred),(succ.succ,pred),
                     (succ,succ.succ),(pred,succ.succ),(pred.pred,succ),(pred.pred,pred)]

bishopDirs = toDirs [(pred,pred),(pred,succ),(succ,pred),(succ,succ)]

kingDirs = rockDirs ++ bishopDirs

pawnNormDirs = toDirs [(id,succ)]
pawnTakingDirs = toDirs [(pred,pred),(succ,succ)]



-- Moves

normMove p pos pos' = Move p pos pos' False
takingMove p pos pos' = Move p pos pos' True

boardAfterMove (Board brd) (Move _ pos pos' _)
    = Board $ brd // [(pos, Nothing), (pos', piece)]
      where piece = brd ! pos



data MoveRule = MoveAndTakeLine | MoveAndTakeOnce | PawnNormMove | PawnTakingMove
                deriving (Show,Eq)

ruleSaysAllLine MoveAndTakeLine = True
ruleSaysAllLine _               = False

ruleSaysCanMove = (`elem` [MoveAndTakeLine,MoveAndTakeOnce,PawnNormMove])
ruleSaysCanTake = (`elem` [MoveAndTakeLine,MoveAndTakeOnce,PawnTakingMove])

possibleMovesDir dir pos0 pos piece board rule
    = case dir pos of
        Just pos' -> case board `at` pos' of
                       Nothing       -> goNorm pos'
                       Just (col',_) -> goTake pos' col'
        Nothing -> []
    where
      Just (col,_) = board `at` pos0
      goNorm pos' = adjNormMoves pos' ++ restNormMoves pos'
      adjNormMoves pos' = [ normMove piece pos0 pos' | ruleSaysCanMove rule ]
      restNormMoves pos' = if ruleSaysAllLine rule then possibleMovesDir dir pos0 pos' piece board rule
                           else []

      goTake pos' col' = [ takingMove piece pos0 pos' | ruleSaysCanTake rule && col /= col' ]

possibleMoves pos piece board rule dirs
    = concatMap (\dir -> possibleMovesDir dir pos pos piece board rule) dirs



moves' Rock pos board   = possibleMoves pos Rock board MoveAndTakeLine rockDirs
moves' Knight pos board = possibleMoves pos Knight board MoveAndTakeOnce knightDirs
moves' Bishop pos board = possibleMoves pos Bishop board MoveAndTakeLine bishopDirs
moves' Queen pos board  = possibleMoves pos Queen board MoveAndTakeLine kingDirs
moves' King pos board   = possibleMoves pos King board MoveAndTakeOnce kingDirs
moves' Pawn pos board   = possibleMoves pos Pawn board PawnNormMove pawnNormDirs
                          ++ possibleMoves pos Pawn board PawnTakingMove pawnTakingDirs

moves pos board
    | Just (_,piece) <- board `at` pos = moves' piece pos board
    | otherwise                        = []


-- Main Logic

canAttack board from to = any canTake ms
    where ms = moves from board
          canTake (Move _ _ to True) = True
          canTake _ = False

isThereCheck colour board = any (\pos -> canAttack board pos kingPos) enemyPos
    where enemyPos = positionsWithNotColour colour board
          kingPos = positionOf (colour,King)


isThereCheckmate colour board = not $ any (\from -> any noCheckAfter $ moves from board) poses
    where
      poses = positionsWithColour colour board
      noCheckAfter = not . isThereCheckAfter colour board


isItAfter what board move = what board' where board' = boardAfterMove board move

isThereCheckAfter colour board move = isItAfter (isThereCheck colour) board move
isThereCheckmateAfter colour board move = isItAfter (isThereCheckmate colour) board move

oneMoveCheckmate board = filter (isThereCheckmateAfter Black board) $ wMoves
    where poses = positionsWithColour White board
          wMoves = concatMap (\pos -> moves pos board) $ poses
