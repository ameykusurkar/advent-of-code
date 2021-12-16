import Data.List 

main = do
  chunks <- fmap lines getContents
  print $ solve1 chunks
  print $ solve2 chunks

data Result = Valid | Incomplete String | Corrupted Char deriving Show

solve1 :: [String] -> Int
solve1 = sum . map (scoreCorrupted . validate)

solve2 :: [String] -> Int
solve2 = median . filter (>0) . map (scoreIncomplete . validate)
  where median xs = sort xs !! div (length xs) 2

scoreCorrupted :: Result -> Int
scoreCorrupted (Corrupted ')') = 3
scoreCorrupted (Corrupted ']') = 57
scoreCorrupted (Corrupted '}') = 1197
scoreCorrupted (Corrupted '>') = 25137
scoreCorrupted _               = 0

scoreIncomplete :: Result -> Int
scoreIncomplete (Incomplete chars) = foldl f 0 chars
  where f acc ')' = acc * 5 + 1
        f acc ']' = acc * 5 + 2
        f acc '}' = acc * 5 + 3
        f acc '>' = acc * 5 + 4
        f _   _   = undefined
scoreIncomplete _                  = 0

validate :: String -> Result
validate chunk = process chunk []

process :: String -> String -> Result
process "" [] = Valid
process "" (e:expected) = Incomplete (e:expected)
process (c:chunk) stack
  | isOpen c  = process chunk (close c:stack) 
  | c == head stack = process chunk (tail stack)
  | otherwise = Corrupted c

isOpen :: Char -> Bool
isOpen = (`elem` "[{<(")

close :: Char -> Char
close '[' = ']'
close '(' = ')'
close '<' = '>'
close '{' = '}'
close _   = undefined
