{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M

main = do
  template <- getLine
  getLine
  rules <- fmap (map parseRule . lines) getContents
  -- print template
  -- print rules
  print $ solve1 10 (M.fromList rules) template

parseRule :: String -> (String, Char)
parseRule str = (to, head from)
  where [to, "->", from] = words str

solve1 :: Int -> M.Map String Char -> String -> Int
solve1 n rules tmp = maxCount - minCount
  where freq = buildCounter output
        output = repeatStep n rules tmp
        maxCount = (maximum . map snd . M.toList) freq
        minCount = (minimum . map snd . M.toList) freq

buildCounter :: Ord a => [a] -> M.Map a Int
buildCounter = M.fromListWith (+) . map (,1)

repeatStep :: Int -> M.Map String Char -> String -> String
repeatStep n rules = apply n (step rules)
  where apply n f = foldl (.) id (replicate n f)

step :: M.Map String Char -> String -> String
step rules tmp = joinTemplate $ map f (pairwise tmp)
  where f (l, r) = l : M.findWithDefault '$' [l, r] rules : [r]

joinTemplate :: [String] -> String
joinTemplate = foldr1 f
  where f [a, b, c] acc = a:b:acc
        f _         _         = undefined

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (tail xs)
