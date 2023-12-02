module Main where

main = do
  parsed <- fmap (map parse . lines) getContents
  print (solve1 [12, 13, 14] parsed)
  print (solve2 parsed)

type Game = (Int, [RGB])
type RGB = [Int]

parse :: String -> Game
parse str = (i, sets)
  where i = read (drop 5 gameNo) -- First 5 chars are "Game "
        sets = map parseRGB $ splitOn ';' $ tail gameData
        (gameNo, gameData) = break (==':') str

parseRGB :: String -> RGB
parseRGB = foldl1 (zipWith (+)) . map getCol . splitOn ','
  where getCol set = case words set of
          [n, "red"]   -> [read n, 0, 0]
          [n, "green"] -> [0, read n, 0]
          [n, "blue"]  -> [0, 0, read n]

solve1 :: RGB -> [Game] -> Int
solve1 constraint = sum . map fst . filter possible
  where possible (_, sets) = all withinConstraint sets
          where withinConstraint set = and $ zipWith (<=) set constraint

solve2 :: [Game] -> Int
solve2 = sum . map (product . minRGB)
  where minRGB (_, sets) = foldl1 (zipWith max) sets

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]
