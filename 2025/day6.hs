import Data.List

main = do
  raw <- getContents
  -- Part 1
  let parsed = parse raw
  print $ sum $ map solve parsed
  -- Part 2
  let parsed2 = parse2 raw
  print $ sum $ map solve parsed2

parse :: String -> [(String, [Int])]
parse raw = map (parse' . reverse) input
  where
    parse' (op : nums) = (op, map read nums)
    input = transpose $ map words $ lines raw

parse2 :: String -> [(String, [Int])]
parse2 raw = zip (words ops) num4
  where
    (ops : num) = lines $ reverse raw
    num2 = transpose num
    num3 = splitOn (all (== ' ')) num2
    num4 = map build num3
    build = map (read . reverse)

solve :: (String, [Int]) -> Int
solve ("+", nums) = sum nums
solve ("*", nums) = product nums

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f str = case break f str of
  (frag, _ : rem) -> frag : splitOn f rem
  (frag, []) -> [frag]
