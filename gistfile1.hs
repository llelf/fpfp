-- MIND YOUR EYES! DIRTY CODE AHEAD.

import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import System
import System.IO
import Data.List
import Data.Ord
import Data.Maybe

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

vocAdj = (M.!)

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
                              
bfs adj target src = go [src] (S.singleton src) M.empty
    where go [] vis r = maybe (Just r) (const Nothing) target
          go (v:q) vis r | Just t <- target, t==v = Just r'
                         | otherwise              = go (q ++ tos) vis' r'
              where (r',tos,vis') = visit (adj v) r [] vis
                    visit [] r' tos vis'    = (r', tos, S.insert v vis')
                    visit (q:qs) r' tos vis'
                        | q `S.notMember` vis' = visit qs (M.insert q v r') (q:tos) (S.insert q vis')
                        | otherwise            = visit qs r' tos vis'


mutations voc a b
    | Just tree <- bfs (vocAdj voc) (Just b) a
                = Just . (a:) . reverse . takeWhile (/=a) . iterate (tree M.!) $ b
    | otherwise
                = Nothing


revTree = M.fromListWith (++) . map (\(k,v) -> (v,[k])) . M.toList


farthestFrom voc from = go [from] 0
    where go vs level = maximumBy (comparing fst) $ map go' vs
              where go' v | v `M.member` tree' = go (tree' M.! v) (level+1)
                          | otherwise          = (level,v)
          tree' = revTree . fromJust . bfs (vocAdj voc) Nothing $ from

maxMutations voc = (lev + 1, mutations voc from to)
    where (from,(lev,to)) = maximumBy (comparing $ fst.snd) . map (ap (,) $ farthestFrom voc) $ words
          words = M.keys voc


printMuts = putStrLn . out
    where out (Just vs) = unwords . intersperse "→" $ vs
          out Nothing   = "NO WAY!"


solve voc from to
    = do  
          hSetEncoding stdout utf8
          printMuts $ mutations voc from to
          print $ maxMutations voc

main = do v1 <- voc "voc1.txt"
          let (n,mut) = maxMutations v1
          print n
          printMuts mut


