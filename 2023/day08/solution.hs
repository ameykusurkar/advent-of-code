module Main where

import Data.Map (Map, fromList, lookup, keys)
import Data.Maybe (fromJust)

main = do
  dirs <- fmap cycle getLine
  getLine
  m <- fmap (fromList . map parse . lines) getContents
  let solve = foldl1 lcm . map (cycleLength m . (dirs,))
  let nodes = filter (endsWith 'A') $ keys m
  print (solve ["AAA"])
  print (solve nodes)

type MyMap = Map String (String, String)
type Point = ([Char], String)

parse :: String -> (String, (String, String))
parse str = (take 3 str, (l, r))
  where
    val = drop 7 str
    l = take 3 val
    r = take 3 $ drop 5 val

cycleLength :: MyMap -> Point -> Int
cycleLength m = length . takeWhile notTerminal . iterate (step m)
  where
    notTerminal = not . endsWith 'Z' . snd

step :: MyMap -> Point -> Point
step m (d:ds, pos) = (ds, lookup pos)
  where
    lookup = fetch d . fromJust . (`Data.Map.lookup` m)
    fetch 'L' = fst
    fetch 'R' = snd

endsWith c s = s !! 2 == c
