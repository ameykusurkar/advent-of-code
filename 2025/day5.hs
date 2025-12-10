main = do
  raw <- getContents
  let (ranges, ids) = parse raw
  print $ solve ranges ids

type Range = (Int, Int)

parse :: String -> ([Range], [Int])
parse raw = (map parseRange rawRanges, map read rawIds)
  where
    [rawRanges, rawIds] = splitOn "" $ lines raw

parseRange :: String -> Range
parseRange = (\[a, b] -> (read a, read b)) . splitOn '-'

count p = length . filter p

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _ : rem) -> frag : splitOn c rem
  (frag, []) -> [frag]

solve :: [Range] -> [Int] -> Int
solve ranges = count (inSomeRange ranges)

inRange :: Range -> Int -> Bool
inRange (a, b) x = a <= x && x <= b

inSomeRange :: [Range] -> Int -> Bool
inSomeRange ranges x = any (`inRange` x) ranges
