module Main where

import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromJust)

main = do
  dirs <- getLine
  getLine
  parsed <- fmap (map parse . lines) getContents
  let m = fromList parsed
  print $ solve m ["AAA"] (cycle dirs)
  -- let starts = filter (endsWith 'A') $ map fst parsed
  -- print $ solve m starts (cycle dirs)

type MyMap = Map String (String, String)

parse :: String -> (String, (String, String))
parse str = (take 3 str, (l, r))
  where
    val = drop 7 str
    l = take 3 val
    r = take 3 $ drop 5 val

solve :: MyMap -> [String] -> [Char] -> Int
solve m locs (d : ds)
  | all (endsWith 'Z') locs = 0
  | otherwise = 1 + solve m locs' ds
  where
    locs' = map solveOne locs
    solveOne = fetch d . fromJust . (`Data.Map.lookup` m)
    fetch 'L' = fst
    fetch 'R' = snd

endsWith c s = s !! 2 == c
