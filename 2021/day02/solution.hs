main = do
  movements <- fmap parse getContents
  print $ solve1 movements
  print $ solve2 movements

data Movement = Up Int | Down Int | Forward Int

parse :: String -> [Movement]
parse = (map parseLine) . lines
  where
    parseLine line = case words line of
      ["up", n] -> Up (read n)
      ["down", n] -> Down (read n)
      ["forward", n] -> Forward (read n)

solve1 :: [Movement] -> Int
solve1 ms = horizontal * depth
  where
    (horizontal, depth) = foldl moveStep (0, 0) ms

solve2 :: [Movement] -> Int
solve2 ms = horizontal * depth
  where
    (horizontal, depth, _aim) = foldl aimStep (0, 0, 0) ms

moveStep :: (Int, Int) -> Movement -> (Int, Int)
moveStep (h, d) (Up x)      = (h, d - x)
moveStep (h, d) (Down x)    = (h, d + x)
moveStep (h, d) (Forward x) = (h + x, d)

aimStep :: (Int, Int, Int) -> Movement -> (Int, Int, Int)
aimStep (h, d, a) (Up x)      = (h, d, a - x)
aimStep (h, d, a) (Down x)    = (h, d, a + x)
aimStep (h, d, a) (Forward x) = (h + x, d + a * x, a)
