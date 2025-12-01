eg = [0, 1, 10, 99, 999]

input :: [Int]
input = [5688, 62084, 2, 3248809, 179, 79, 0, 172169]

solve :: Int -> [Int] -> Int
solve n = length . last . take (n + 1) . (iterate $ foldMap step)

step :: Int -> [Int]
step 0 = [1]
step n
  | even $ digits n = split n
  | otherwise = [n * 2024]

digits :: Int -> Int
digits = length . show

split :: Int -> [Int]
split n = [read l, read r]
  where
    (l, r) = splitAt mid (show n)
    mid = digits n `div` 2
