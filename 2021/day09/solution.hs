{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

import Data.Char
import Data.List

type Window a = Triple (Triple a)
data Triple a = Triple a a a deriving (Foldable, Functor, Show)

main = do
  entries <- fmap (map parseLine . lines) getContents
  print $ solve1 entries

parseLine :: String -> [Int]
parseLine = map digitToInt

solve1 :: [[Int]] -> Int
solve1 = sum .map (+1) . getLowPoints

getLowPoints :: [[Int]] -> [Int]
getLowPoints = concatMap (map middle . filter isLowPoint) . windows
  where windows = window . surround 9

inThrees :: [a] -> [Triple a]
inThrees [] = undefined
inThrees (x:xs) = map toTriple $ zip3 (x:xs) xs (tail xs)
  where toTriple (a, b, c) = Triple a b c

window :: [[a]] -> [[Window a]]
window = map inThrees . transpose . map inThrees

surround :: a -> [[a]] -> [[a]]
surround x = map (surroundRow x) . transpose . map (surroundRow x)
  where surroundRow x xs = x : xs ++ [x]

isLowPoint :: Window Int -> Bool
isLowPoint w@(Triple (Triple _ t _) (Triple l mid r) (Triple _ b _)) = mid < minimum [t, b, l, r]

middle :: Window a -> a
middle (Triple _ (Triple _ mid _) _) = mid
