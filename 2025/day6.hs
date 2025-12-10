import Data.List

main = do
  raw <- getContents
  let input = transpose $ map words $ lines raw
  let parsed = map (parse . reverse) input
  print $ sum $ map solve parsed

solve :: (String, [Int]) -> Int
solve ("+", nums) = sum nums
solve ("*", nums) = product nums

parse :: [String] -> (String, [Int])
parse (op : nums) = (op, map read nums)
