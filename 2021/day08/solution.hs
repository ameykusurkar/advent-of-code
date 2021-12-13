import Data.List

type Digit = String
type Entry = ([Digit], [Digit])

main = do
  entries <- fmap (map parseLine . lines) getContents
  print $ solve1 entries

parseLine :: String -> Entry
parseLine line = (words signals, words output)
  where [signals, output] = splitOn '|' line

solve1 :: [Entry] -> Int
solve1 =  count (hasLengths [2, 4, 3, 7]) . concatMap snd

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

hasLengths :: [Int] -> [a] -> Bool
hasLengths lens = or . mapM (==) lens . length
