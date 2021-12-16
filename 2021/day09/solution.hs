{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

import Data.Char
import Data.List

type Window a = Triple (Triple a)
data Triple a = Triple a a a deriving (Foldable, Functor, Show)

data Node = Node { getDir :: Direction, getC :: Int, getV :: Int }
data Direction = U | D | L | R | S | W deriving Eq

main = do
  entries <- fmap (map2d digitToInt . lines) getContents
  print $ solve1 entries
  print $ solve2 entries

solve1 :: [[Int]] -> Int
solve1 = sum .map (+1) . getSinks

solve2 :: [[Int]] -> Int
solve2 = product . take 3 . reverse . sort . map getV . sinks
  where sinks = concat . filter2d isSink . reduced
        isSink = (== S) . getDir
        reduced = repeatReduceNode . init
        init = map2d initNode . window W . map2d initDir . window 9

getSinks :: [[Int]] -> [Int]
getSinks = concat . map2d middle . filter2d isSink . window 9
  where isSink = (== S) . initDir
        middle (Triple _ (Triple _ mid _) _) = mid

inThrees :: [a] -> [Triple a]
inThrees [] = undefined
inThrees (x:xs) = map toTriple $ zip3 (x:xs) xs (tail xs)
  where toTriple (a, b, c) = Triple a b c

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d = map . map

filter2d :: (a -> Bool) -> [[a]] -> [[a]]
filter2d = map . filter

window :: a -> [[a]] -> [[Window a]]
window wall = map inThrees . transpose . map inThrees . surround wall

surround :: a -> [[a]] -> [[a]]
surround x = map (surroundRow x) . transpose . map (surroundRow x)
  where surroundRow x xs = x : xs ++ [x]

initDir :: Window Int -> Direction
initDir (Triple (Triple _ l _) (Triple t mid b) (Triple _ r _))
  | mid == 9 = W
  | null cands = S
  | small == t = U
  | small == b = D
  | small == l = L
  | small == r = R
  | otherwise = undefined
  where small = minimum cands
        cands = filter (<mid) [t, b, l, r]

initNode :: Window Direction -> Node
initNode w@(Triple (Triple _ l _) (Triple t mid b) (Triple _ r _)) = Node mid incoming 1
  where incoming = countIncoming w

countIncoming :: Window Direction -> Int
countIncoming w@(Triple (Triple _ l _) (Triple t mid b) (Triple _ r _)) = incoming
  where incoming = count id [t == D, b == U, l == R, r == L]

repeatReduceNode :: [[Node]] -> [[Node]]
repeatReduceNode ns = if noDirs ns
                      then ns
                      else repeatReduceNode (map2d reduceNode wNode)
  where wNode = window (Node W 0 0) ns
        noDirs = null . concat . filter2d hasDir
        hasDir (Node d _ _) = d `elem` [U, D, L, R]

reduceNode :: Window Node -> Node
reduceNode (Triple (Triple _ l _) (Triple t mid b) (Triple _ r _)) = transform mid
  where transform (Node S 0 v) = Node S 0 v
        transform (Node _ 0 _) = Node W 0 0
        transform (Node dir c v) = if or ready then Node dir (c - count id ready) (v + sumV) else Node dir c v
        ready = [ dirT == D && cT == 0
                , dirB == U && cB == 0
                , dirL == R && cL == 0
                , dirR == L && cR == 0 ]
        sumV = sum [ if dirT == D && cT == 0 then vT else 0
                   , if dirB == U && cB == 0 then vB else 0
                   , if dirL == R && cL == 0 then vL else 0
                   , if dirR == L && cR == 0 then vR else 0 ]
        [Node dirT cT vT, Node dirB cB vB, Node dirL cL vL, Node dirR cR vR] = [t, b, l, r]

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred
