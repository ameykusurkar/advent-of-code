module Main where

import Data.List (sort)
import Data.Monoid

main = do
  raw <- getContents
  let input = map read $ lines raw
  putStr $ show $ combs (0: sort input)

solve input = diffs 0 0 $ (0 : sort input) ++ [m]
  where
    m = maximum input + 3

diffs ones threes (x1 : x2 : xs)
  | x2 - x1 == 1 = diffs (ones + 1) threes (x2 : xs)
  | x2 - x1 == 3 = diffs ones (threes + 1) (x2 : xs)
  | otherwise = diffs ones threes (x2 : xs)
diffs ones threes _ = ones * threes

cands :: Int -> [Int] -> [[Int]]
cands curr [] = []
cands curr (x : xs)
  | x - curr <= 3 = (x : xs) : (cands curr xs)
  | x - curr > 3 = []

combs :: [Int] -> Sum Int
combs [curr] = Sum 1
combs (curr : xs) = foldMap combs (cands curr xs)
