main = do
  nums <- fmap parse getContents
  print $ solve1 nums
  print $ solve2 nums

parse :: String -> [Int]
parse = (map read) . lines

solve1 :: [Int] -> Int
solve1 = countIncreasing

solve2 :: [Int] -> Int
solve2 = countIncreasing . map sum . threewise
  where sum (a, b, c) = a + b + c

countIncreasing :: [Int] -> Int
countIncreasing xs = count (uncurry (<)) (pairwise xs)

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (tail xs)

threewise :: [a] -> [(a, a, a)]
threewise (x:xs) = zip3 (x:xs) xs (tail xs)

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred
