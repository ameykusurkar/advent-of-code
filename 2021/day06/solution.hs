{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M

type Fish = Counter Int
type Counter k = M.Map k Int

main = do
  nums <- fmap parse getLine
  print $ simulate 80 nums
  print $ simulate 256 nums

parse :: String -> [Int]
parse = map read . splitOn ','

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]

apply :: Int -> (a -> a) -> a -> a
apply n f = foldl (.) id (replicate n f)

simulate :: Int -> [Int] -> Int
simulate n = sum . apply n step . intoCounter
  where intoCounter = M.fromListWith (+) . map (,1)

step :: Fish -> Fish
step = M.unionsWith (+) . M.mapWithKey stepAge
  where stepAge 0 n = M.fromList [(8, n), (6, n)]
        stepAge a n = M.fromList [(a - 1, n)]
