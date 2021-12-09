main = do
  nums <- fmap parse getContents
  print $ solve1 nums
  print $ solve2 nums

parse :: String -> [Int]
parse = (map read) . lines

solve1 :: [Int] -> Int
solve1 xs = count isIncreasing (pairwise xs)

solve2 :: [Int] -> Int
solve2 xs = count isIncreasing (pairwise triples)
  where triples = map (\(a, b, c) -> a + b + c) (threewise xs)

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (tail xs)

threewise :: [a] -> [(a, a, a)]
threewise (x:xs) = zip3 (x:xs) xs (tail xs)

isIncreasing :: (Int, Int) -> Bool
isIncreasing (a, b) = b > a

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred
