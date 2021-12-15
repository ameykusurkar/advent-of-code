{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

import Data.Char
import Data.List

type Window a = Triple (Triple a)
data Triple a = Triple a a a deriving (Foldable, Functor, Show)

data Node = Node { getDir :: Direction, getC :: Int, getV :: Int }
data Direction = U | D | L | R | S | W deriving (Eq, Show)

instance Show Node where
  show (Node W _ _) = "     "
  show (Node dir c v) = show dir ++ " " ++ show c ++ " " ++ show v

main = do
  raw <- getContents
  let entries = (map parseLine . lines) raw
  print $ solve1 entries
  print (solve2 (-55) entries)

parseLine :: String -> [Int]
parseLine = map digitToInt

solve1 :: [[Int]] -> Int
solve1 = sum .map (+1) . getLowPoints

solve2 :: Int -> [[Int]] -> Int
solve2 lim = product . take 3 . reverse . sort . map getV . sinks
  where sinks = concat . filter2d isSink . reduced
        isSink (Node d _ _) = d == S
        reduced = repeatReduceNode lim . init
        init = map2d initNode . window . surround W . map2d pointD . window . surround 9

solve0 :: [[Int]] -> [[Char]]
solve0 = map2d point . window . surround 9

getLowPoints :: [[Int]] -> [Int]
getLowPoints = concat . map2d middle . filter2d isLowPoint . window . surround 9

inThrees :: [a] -> [Triple a]
inThrees [] = undefined
inThrees (x:xs) = map toTriple $ zip3 (x:xs) xs (tail xs)
  where toTriple (a, b, c) = Triple a b c

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d = map . map

filter2d :: (a -> Bool) -> [[a]] -> [[a]]
filter2d = map . filter

window :: [[a]] -> [[Window a]]
window = map inThrees . transpose . map inThrees

surround :: a -> [[a]] -> [[a]]
surround x = map (surroundRow x) . transpose . map (surroundRow x)
  where surroundRow x xs = x : xs ++ [x]

isLowPoint :: Window Int -> Bool
isLowPoint w@(Triple (Triple _ t _) (Triple l mid r) (Triple _ b _)) = mid < minimum [t, b, l, r]

pointD :: Window Int -> Direction
pointD (Triple (Triple _ l _) (Triple t mid b) (Triple _ r _))
  | mid == 9 = W
  | null cands = S
  | small == t = U
  | small == b = D
  | small == l = L
  | small == r = R
  | otherwise = undefined
  where small = minimum cands
        cands = filter (<mid) [t, b, l, r]

repeatReduceNode :: Int -> [[Node]] -> [[Node]]
-- repeatReduceNode 0 ns = ns
repeatReduceNode limit ns = if noDirs ns
                        then ns
                        else repeatReduceNode (limit - 1) (map2d reduceNode wNode)
  where wNode = (window . surround wall) ns
        wall = Node W 0 0 
        noDirs = null . concat . filter2d hasDir
        -- takeLeaves = concat . map2d extractLeaf
        -- extractLeaf (Leaf x) = x 
        -- extractLeaf _        = undefined
        -- noBranches = null . concat . filter2d (not . isBranch)

hasDir :: Node -> Bool
hasDir (Node d _ _) = d `elem` [U, D, L, R]

point :: Window Int -> Char
point (Triple (Triple _ l _) (Triple t mid b) (Triple _ r _))
  | mid == 9   = ' '
  | isLeaf mid = 'x'
  | otherwise  = 'o'
  where isLeaf x = all (\n -> n == 9 || x > n) cands
        cands = [t, b, l, r]

initNode :: Window Direction -> Node
initNode w@(Triple (Triple _ l _) (Triple t mid b) (Triple _ r _)) = Node mid incoming 1
  where incoming = countIncoming w

countIncoming :: Window Direction -> Int
countIncoming w@(Triple (Triple _ l _) (Triple t mid b) (Triple _ r _)) = count id incoming
  where incoming = [t == D, b == U, l == R, r == L]

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

middle :: Window a -> a
middle (Triple _ (Triple _ mid _) _) = mid

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred
