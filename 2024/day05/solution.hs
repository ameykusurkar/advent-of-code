module Main where

main = do
  raw <- getContents
  let (rawRules, rawUpdates') = span (/= "") $ lines raw
  let rawUpdates = drop 1 rawUpdates'
  let rules = map parseRule rawRules
  let updates = map parseUpdate rawUpdates
  print $ solve rules updates
  print $ solve2 rules updates

parseRule :: String -> (Int, Int)
parseRule str = (read a, read $ drop 1 b)
  where
    (a, b) = span (/= '|') str

parseUpdate :: String -> [Int]
parseUpdate str = read $ '[' : str ++ "]"

solve rules updates = sum $ map middle validUpdates
  where
    validUpdates = filter (valid rules) updates

solve2 rules updates = sum $ map middle fixedUpdates
  where
    fixedUpdates = map (fix rules) invalidUpdates
    invalidUpdates = filter (not . valid rules) updates

valid :: [(Int, Int)] -> [Int] -> Bool
valid rules update = all (`elem` rules) (pairwise update)

pairwise xs = zip xs (drop 1 xs)

middle xs = xs !! n
  where
    n = div (length xs) 2

fix rules updates
  | valid rules updates = updates
  | otherwise = fix rules (swap rules updates)

swap :: [(Int, Int)] -> [Int] -> [Int]
swap rules [x] = [x]
swap rules (a : b : xs)
  | elem (b, a) rules = b : swap rules (a : xs)
  | otherwise = a : swap rules (b : xs)
