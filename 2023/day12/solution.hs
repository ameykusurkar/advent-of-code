module Main where

main = do
  raw <- fmap lines getContents
  let inputs = map parse raw
  -- print inputs
  print $ sum $ map (uncurry solve) inputs
  -- print $ map (uncurry solve) inputs

parse :: String -> ([Char], [Int])
parse raw = (springs, counts)
  where [springs, rawCounts] = words raw
        counts = map read $ splitOn ',' rawCounts

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _ : rem) -> frag : splitOn c rem
  (frag, []) -> [frag]

solve :: [Char] -> [Int] -> Int
solve ('.' : ss) ns       = solve ss ns
solve ('?' : ss) ns       = solve ('#' : ss) ns + solve ('.' : ss) ns
solve ('#' : ss) (n : ns)
  | hashes >  n                    = 0
  | possib <  n && null rem        = 0
  | possib <  n && head rem == '.' = 0
  | possib <  n                    = solve rem (n-possib:ns)
  | possib >= n && null rem        = solve rem ns
  | possib >= n && head rem == '#' = 0
  | possib >= n                    = solve ('.':drop n ss) ns
  where hashes = 1 + length (takeWhile (=='#') ss)
        possib = 1 + length (takeWhile (=='?') ss)
        rem = drop (min possib n) ('#' : ss)
solve []         [] = 1
solve (s:ss)     [] = 0
solve []         (n : ns) = 0
solve ss ns = error $ show ss ++ show ns
