module Main where

import Data.Char
import Data.List

main = do
  raw <- getContents
  let input = parse raw
  print $ solve1 input
  let [targets, items] = input
  print $ solve2 items targets

parse :: String -> [[Int]]
parse = transpose . map (map read . list . span isDigit) . lines

list (a, b) = [a, b]
pair [a, b] = (a, b)

solve1 :: [[Int]] -> Int
solve1 = sum . map abs . uncurry (zipWith (-)) . pair . map sort

solve2 :: [Int] -> [Int] -> Int
solve2 items = sum . map similarity
  where
    similarity x = x * count x items
    count a = length . filter (== a)
