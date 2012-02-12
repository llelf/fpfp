
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
    show (Pos file dist) = [['a'..'h'] !! fromEnum file] ++ show dist

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
board `colourAt` pos = do { (colour,_) <- board `at` pos; return colour }

boardPieces (Board brd) = [ (pos, x) | (pos, Just x) <- assocs brd ]

positionsBy by = map (\(pos,_) -> pos) . filter (\(_,p) -> by p) . boardPieces
positionsWithColour c = positionsBy (\(c',_) -> c == c')
positionsWithNotColour c board = positionsBy (const True) board \\ positionsWithColour c board

positionOf it = listToMaybe . positionsBy (==it)



-- Move directions

toDirs :: [(Int->Int, Int->Int)] -> [Pos -> Maybe Pos]
toDirs = map (\(fdo,rdo) -> coordsToPos . (fdo***rdo) . posToCoords)
    where posToCoords (Pos file dist) = (fromEnum file, dist - 1)
          coordsToPos (x,y) | inRange ((0,0),(7,7)) (x,y) = Just $ Pos (toEnum x) (y+1)
                            | otherwise                   = Nothing

rockDirs = toDirs [(pred,id),(succ,id),(id,pred),(id,succ)]

knightDirs = toDirs [(pred,pred.pred),(succ,pred.pred),(succ.succ,pred),(succ.succ,pred),
                     (succ,succ.succ),(pred,succ.succ),(pred.pred,succ),(pred.pred,pred)]

bishopDirs = toDirs [(pred,pred),(pred,succ),(succ,pred),(succ,succ)]

kingDirs = rockDirs ++ bishopDirs

pawnNormDirs = toDirs [(id,succ)]
pawnTakingDirs = toDirs [(pred,pred),(succ,succ)]


-- Moves
-- NB Much simplified rules

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
    | Just pos' <- dir pos = case board `at` pos' of
                               Nothing       -> goNorm pos'
                               Just (col',_) -> goTake pos' col'
    | otherwise            = []
    where
      Just col = board `colourAt` pos0
      goNorm pos' = adjNormMoves pos' ++ restNormMoves pos'
      adjNormMoves pos' = [ normMove piece pos0 pos' | ruleSaysCanMove rule ]
      restNormMoves pos' = if ruleSaysAllLine rule then possibleMovesDir dir pos0 pos' piece board rule
                           else []

      goTake pos' col' = [ takingMove piece pos0 pos' | ruleSaysCanTake rule && col /= col' ]

possibleMoves pos piece board rule dirs ch
    = filter checkSafe . concatMap (\dir -> possibleMovesDir dir pos pos piece board rule) $ dirs
      where checkSafe = if ch then not . isThereCheckAfter col board
                        else const True
            Just col = board `colourAt` pos



moves' Rock pos board ch   = possibleMoves pos Rock board MoveAndTakeLine rockDirs ch
moves' Knight pos board ch = possibleMoves pos Knight board MoveAndTakeOnce knightDirs ch
moves' Bishop pos board ch = possibleMoves pos Bishop board MoveAndTakeLine bishopDirs ch
moves' Queen pos board ch = possibleMoves pos Queen board MoveAndTakeLine kingDirs ch
moves' King pos board ch  = possibleMoves pos King board MoveAndTakeOnce kingDirs ch
moves' Pawn pos board ch  = possibleMoves pos Pawn board PawnNormMove pawnNormDirs ch
                          ++ possibleMoves pos Pawn board PawnTakingMove pawnTakingDirs ch


movesChecking check pos board
    | Just (_,piece) <- board `at` pos = moves' piece pos board check
    | otherwise                        = []

unsafeMoves = movesChecking False
moves = movesChecking True



-- Main Logic

canAttack board from to = any canTake ms -- it was bad idea really
    where ms = unsafeMoves from board
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
