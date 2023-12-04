module Main where

import Data.List

main = do
  parsed <- fmap (map parse . lines) getContents
  print (solve1 parsed)
  print (solve2 parsed)

type Card = ([Int], [Int])

parse :: String -> Card
parse str = (winners, cands)
  where
    [winners, cands] = map readElems (splitOn '|' rest)
    [_, rest] = splitOn ':' str
    readElems = map read . words

solve1 :: [Card] -> Int
solve1 = sum . map (score . wins)
  where
    score 0 = 0
    score n = 2 ^ (n - 1)

solve2 :: [Card] -> Int
solve2 = sum . map snd . countCopies . map ((,1) . wins)

wins :: Card -> Int
wins (winners, cands) = length $ filter (`elem` winners) cands

countCopies :: [(Int, Int)] -> [(Int, Int)]
countCopies [] = []
countCopies ((wins, count) : rest) = (wins, count) : countCopies rest'
  where
    rest' = mapN wins (\(w, c) -> (w, c + count)) rest

mapN n f xs = map f firstN ++ rest
  where (firstN, rest) = splitAt n xs

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _ : rem) -> frag : splitOn c rem
  (frag, []) -> [frag]
