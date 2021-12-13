import Data.List

main = do
  positions <- fmap parse getLine
  print $ solve absDist positions
  print $ solve incDist positions

parse :: String -> [Int]
parse = map read . splitOn ','

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]

solve :: (Int -> Int -> Int) -> [Int] -> Int
solve distF ps = minimum $ map (sum . (`changes` ps)) [minimum ps .. maximum ps]
  where changes n = map (distF n)

absDist :: Int -> Int -> Int
absDist a b = abs (a - b)

incDist :: Int -> Int -> Int
incDist a b = sumTo $ absDist a b
  where sumTo n = n * (n + 1) `div` 2 
