main = do
  raw <- getContents
  let ls = map parseLine $ lines raw
  print $ solve try ls
  print $ solve try2 ls

type Eval = Int -> Int -> [Int]

parseLine :: String -> (Int, [Int])
parseLine str = (read target, nums)
  where
    (target, _ : rest) = span (/= ':') str
    nums = map read $ words rest

solve :: Eval -> [(Int, [Int])] -> Int
solve f = sum . map fst . filter valid
  where
    valid (target, xs) = elem target $ eval f (reverse xs)

eval :: Eval -> [Int] -> [Int]
eval f [a, b] = f a b
eval f (x : xs) = eval f xs >>= f x

try :: Int -> Int -> [Int]
try a b = [a + b, a * b]

try2 :: Int -> Int -> [Int]
try2 a b = [a + b, a * b, read $ show b ++ show a]
