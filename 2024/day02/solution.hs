module Main where

main = do
  raw <- getContents
  let reports = map (map read . words) $ lines raw
  print $ count safe reports
  print $ count safe2 reports

count f = length . filter f

safe :: [Int] -> Bool
safe xs = all neg diffs || all pos diffs
  where
    diffs = zipWith (-) xs (drop 1 xs)
    pos x = 1 <= x && x <= 3
    neg x = -3 <= x && x <= -1

safe2 :: [Int] -> Bool
safe2 xs = any safe (withoutOne xs)

withoutOne :: [Int] -> [[Int]]
withoutOne [x] = [[]]
withoutOne (x : xs) = xs : map (x :) (withoutOne xs)
