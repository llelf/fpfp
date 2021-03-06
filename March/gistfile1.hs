
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Set as S

type State = (Integer,Integer)

data Vessel = A | B deriving (Show,Eq)
type World = (Capacity,Capacity)
type Capacity = Integer

data Move = Empty Vessel | Fill Vessel | Pour (Vessel,Vessel)
            deriving (Show,Eq)

(a,b) # A = a
(a,b) # B = b


nextMove cap (from,to) vs | vs#from == 0    = Fill from
                          | vs#to == cap#to = Empty to
                          | otherwise       = Pour (from,to)


(//) :: State -> (Vessel, Integer) -> State
vessels // (v,x) = ((first,second) # v) (const x) vessels


apply :: World -> Move -> State -> State

apply cap (Fill v) ves = ves // (v, cap#v)
apply cap (Empty v) ves = ves // (v, 0)
apply cap (Pour (from,to)) ves = ves // (from, ves#from - x) // (to, ves#to + x)
    where x = min (ves#from) (cap#to - ves#to)


-- iterate pours in direction where we pours from A to B ((A,B)=dir)
iteratePours :: World -> (Vessel,Vessel) -> State -> [(Move,State)]
iteratePours world dir state = unfoldr pour state
    where pour st = Just ((move,st'), st')
              where move = nextMove world dir st
                    st'  = apply world move st


solve :: World -> Integer -> Maybe [Move]

solve world target = minimumResult
    where pours = [ result $ iteratePours world dir (0,0) | dir <- [(A,B), (B,A)] ]

          minimumResult
              | ps <- catMaybes pours, not $ null ps = Just . minimumBy (comparing length) $ ps
              | otherwise                            = Nothing

          result moves = result' moves [] S.empty
          result' ((m,st):rest) moves states
              | hitTarget st     = Just . reverse $ m : moves
              | cycled st states = Nothing
              | otherwise        = result' rest (m : moves) (S.insert st states)

          hitTarget (a,b) = a == target || b == target
          cycled = S.member

main = print [ x | x <- [1..1011], let s = solve (1010,1011) x, length (fromMaybe [] s) == 2012 ]
