module Main where

main = do
  (seeds, maps) <- fmap (parse . filter (not . null) . splitOn "" . splitOn '\n') getContents
  print (solve seeds maps)
  -- print (solve (expandRange seeds) maps)

type Map = [Range]
type Range = [Int]

parse :: [[String]] -> ([Int], [Map])
parse (seedText : sections) = (seeds, rest)
  where
    seeds = readElems $ drop 7 $ head seedText
    rest = map (map readElems . tail) sections
    readElems = map read . words

solve :: [Int] -> [Map] -> Int
solve seeds maps = minimum $ map (follow maps) seeds

-- expandRange :: [Int] -> [Int]
-- expandRange []                 = []
-- expandRange (start:width:rest) = [start..(start+width-1)] ++ expandRange rest

follow :: [Map] -> Int -> Int
follow ms seed = foldl (flip findDest) seed ms

findDest :: Map -> Int -> Int
findDest ([destStart, srcStart, width] : ms) seed
  | srcStart <= seed && seed <= srcStart + width = destStart + (seed - srcStart)
  | otherwise = findDest ms seed
findDest [] seed = seed

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _ : rem) -> frag : splitOn c rem
  (frag, []) -> [frag]
