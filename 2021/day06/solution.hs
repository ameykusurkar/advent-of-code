main = do
  nums <- fmap parse getLine
  print $ length $ (apply 80 step) nums

parse :: String -> [Int]
parse = (map read) . splitOn ','

step :: [Int] -> [Int]
step [] = []
step (0:rest) = 8 : 6 : (step rest)
step (n:rest) = (n - 1) : (step rest)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]

apply :: Int -> (a -> a) -> a -> a
apply n f = foldl (.) id (take n (repeat f))
