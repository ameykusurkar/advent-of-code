module Main where

main = do
  ls <- fmap lines getContents
  let series = map (map read . words) ls
  print $ sum $ map next series
  print $ sum $ map (next . reverse) series

next :: [Int] -> Int
next xs
  | all (== 0) xs = 0
  | otherwise     = inc + last xs 
  where inc = next (diffs xs)
        diffs xs = zipWith (-) (tail xs) xs
