main = do
  (points, folds) <- fmap parse getContents
  print points
  print folds

type Point = (Int, Int)
data Fold = Y Int | X Int deriving Show

parse :: String -> ([Point], [Fold])
parse str = (map parseP pointsRaw, map parseF foldsRaw)
  where [pointsRaw, foldsRaw] = splitOn "" (lines str)
        parseP p = (read l, read r)
          where [l, r] = splitOn ',' p
        parseF f
          | c == 'y'    = Y (read i)
          | otherwise = X (read i)
          where [_, _, c:'=':i] = words f

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]
