main = do
  movements <- fmap parse getContents
  print $ solve1 movements

data Movement = Up Int | Down Int | Forward Int
type Position = (Int, Int) -- (Horizontal, Depth)

parse :: String -> [Movement]
parse = (map parseLine) . lines
  where
    parseLine ('u':'p':' ':n) = Up (read n)
    parseLine ('d':'o':'w':'n':' ':n) = Down (read n)
    parseLine ('f':'o':'r':'w':'a':'r':'d':' ':n) = Forward (read n)

solve1 :: [Movement] -> Int
solve1 ms = horizontal * depth
  where
    (horizontal, depth) = foldl moveStep (0, 0) ms

moveStep :: Position -> Movement -> Position
moveStep (h, d) (Up x)      = (h, d - x)
moveStep (h, d) (Down x)    = (h, d + x)
moveStep (h, d) (Forward x) = (h + x, d)
