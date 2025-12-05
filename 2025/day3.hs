import Data.Char

main = do
  raw <- getContents
  let banks = map parse $ lines raw
  print $ solve banks

solve :: [[Int]] -> Int
solve = sum . map (combine . biggest)
  where
    combine (a, b) = a * 10 + b

biggest :: [Int] -> (Int, Int)
biggest [x, y] = (x, y)
biggest (x : xs) = if x >= a then (x, max a b) else (a, b)
  where
    (a, b) = biggest xs

parse :: String -> [Int]
parse = map digitToInt
