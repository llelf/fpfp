import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import System
import System.IO


letters = ['а'..'я'] ++ ['ё']
--letters = ['a'..'z']


possibleAdjWords w = (\\[w]) . nub . sort . concat . zipWith mutate (inits w) $ tails w
    where mutate _ [] = []
          mutate a (x:b) = [ a ++ y:b | y <- letters ]


type Mutations = M.Map String [String]

add (w,w') dict
    | w' `S.member` dict
        = M.updateWithKey (\_ v -> Just (w:v)) w' . M.updateWithKey (\_ v -> Just (w':v)) w
    | otherwise
        = id


vocBy cons file = readFile file >>= return . cons . lines
voc = vocBy construct

constructBy adj ws = foldl addMutated scratch ws
    where scratch = foldr (flip M.insert []) M.empty ws
          addMutated muts w = foldr (\m -> add (w,m) dict) muts $ adj w
          dict = S.fromList ws

construct = constructBy possibleAdjWords
construct3 = constructBy (\w -> possibleAdjWords w ++ to3 w ++ to4 w)
    where to3 w = concat $ zipWith f (inits w) (tails w)
          f w1 [] = []
          f w1 (w:w2) = [w1++w2]
          to4 w = [ c:w | c <- letters ]
                              
bfs f t s = go [s] (S.singleton s) M.empty
    where go [] vis r = Nothing
          go (v:q) vis r | v==t             = Just r'
                         | otherwise        = go (q ++ tos) vis' r'
              where (r',tos,vis') = visit (f v) r [] vis
                    visit [] r' tos vis'    = (r', tos, S.insert v vis')
                    visit (q:qs) r' tos vis'
                        | q `S.notMember` vis' = visit qs (M.insert q v r') (q:tos) (S.insert q vis')
                        | otherwise            = visit qs r' tos vis'


mutations muts a b
    | Just tree <- bfs f b a = Just . (a:) . reverse . takeWhile (/=a) . iterate (tree M.!) $ b
    | otherwise              = Nothing
    where f w = muts M.! w


solve voc from to
    = do  
          hSetEncoding stdout utf8
          putStrLn . out $ mutations voc from to
              where out (Just vs) = unwords . intersperse "→" $ vs
                    out Nothing   = "NO WAY!"
