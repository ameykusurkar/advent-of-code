{-# LANGUAGE DeriveFoldable, DeriveFunctor, TupleSections #-}

import Data.Char
import Data.List

type Window a = Triple (Triple a)
data Triple a = Triple a a a deriving (Foldable, Functor, Show)

main = do
  raw <- getContents
  let entries = (map2d digitToInt . lines) raw
  print $ solve1 entries

solve1 :: [[Int]] -> Int
solve1 = fst . apply 100 step . (0,)
  where apply n f = foldl (.) id (replicate n f)

step :: (Int, [[Int]]) -> (Int, [[Int]])
step (c, grid) = (c + count2d totalFlashed grid'', map2d reset grid'')
  where addOne x = let x' = x + 1 in (x', x' > 9, x' > 9)
        reset (x, _, _) = if x > 9 then 0 else x
        grid' = map2d addOne grid
        grid'' = propagate grid'

propagate :: [[(Int, Bool, Bool)]] -> [[(Int, Bool, Bool)]]
propagate gr = if numFlashes == 0 then gr' else propagate gr'
  where gr' = (map2d increment . windowed) gr
        windowed = window (0, False, False)
        numFlashes = count2d justFlashed gr'

justFlashed :: (Int, Bool, Bool) -> Bool
justFlashed (_, _, b) = b

totalFlashed :: (Int, Bool, Bool) -> Bool
totalFlashed (_, b, _) = b

increment :: Window (Int, Bool, Bool) -> (Int, Bool, Bool)
increment (Triple (Triple tl l bl) (Triple t (e, f, jf) b) (Triple tr r br)) = (e', f', jf')
  where e' = e + countFlashes neibs
        f' = e' > 9
        jf' = not f && f'
        countFlashes = count id . map justFlashed
        neibs = [tl, l, bl, t, b, tr, r, br]

inThrees :: [a] -> [Triple a]
inThrees [] = undefined
inThrees (x:xs) = map toTriple $ zip3 (x:xs) xs (tail xs)
  where toTriple (a, b, c) = Triple a b c

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d = map . map

count2d :: (a -> Bool) -> [[a]] -> Int
count2d pred = length . concatMap (filter pred)

window :: a -> [[a]] -> [[Window a]]
window wall = map inThrees . transpose . map inThrees . surround wall

surround :: a -> [[a]] -> [[a]]
surround x = map (surroundRow x) . transpose . map (surroundRow x)
  where surroundRow x xs = x : xs ++ [x]

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred
