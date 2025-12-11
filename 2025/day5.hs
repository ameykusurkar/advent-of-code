main = do
  raw <- getContents
  let (ranges, ids) = parse raw
  -- print ranges
  print $ solve ranges ids
  print $ solve2 ranges

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

solve2 :: [Range] -> Int
solve2 = sum . map dist . collapse
  where
    dist (a, b) = b - a + 1

combine :: Range -> Range -> Maybe Range
combine (st1, end1) (st2, end2)
  | (st1 <= end2) && (st2 <= end1) = Just (min st1 st2, max end1 end2)
  | otherwise = Nothing

slot :: [Range] -> Range -> [Range]
slot [] range = [range]
slot (r : rs) range = case combine range r of
  Just r' -> r' : rs
  Nothing -> r : slot rs range

collapse :: [Range] -> [Range]
collapse xs
  | collapsed == xs = xs
  | otherwise = collapse collapsed
  where
    collapsed = foldl slot [] xs
