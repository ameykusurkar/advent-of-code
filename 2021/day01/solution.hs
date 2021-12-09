main = interact (show . solve . parse)

parse :: String -> [Int]
parse = (map read) . lines

solve :: [Int] -> Int
solve xs = count isIncreasing (pairwise xs)

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (tail xs)

isIncreasing :: (Int, Int) -> Bool
isIncreasing (a, b) = b > a

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred
