import qualified Data.Set as S

main = do
  (points, folds) <- fmap parse getContents
  -- The line below gives the solution for part 1:
  -- print $ length $ solve1 (take 1 folds) (S.fromList points)
  --
  --
  -- This prints out points which can be piped into `gnuplot`:
  --
  -- make run 2> /dev/null \
  --   | tail -n +2 \
  --   | gnuplot -p -e 'set size ratio -1;set xrange [-1:42]; set yrange [-1:8]; plot "/dev/stdin"'
  --
  -- The letters a vertically flipped, but they read: JRZBLGKH
  putStrLn $ render $ solve1 folds (S.fromList points)

type Point = (Int, Int)
data Fold = Y Int | X Int deriving Show

render :: S.Set Point -> String
render = unlines . map withSpace . S.toList
  where withSpace (x, y) = show x ++ " " ++ show y

parse :: String -> ([Point], [Fold])
parse str = (map parseP pointsRaw, map parseF foldsRaw)
  where [pointsRaw, foldsRaw] = splitOn "" (lines str)
        parseP p = (read l, read r)
          where [l, r] = splitOn ',' p
        parseF f
          | c == 'y'    = Y (read i)
          | otherwise = X (read i)
          where [_, _, c:'=':i] = words f

reflect :: Fold -> Point -> Point
reflect (Y yf) (x, y) = (x, reflect1D yf y)
reflect (X xf) (x, y) = (reflect1D xf x, y)

reflect1D :: Int -> Int -> Int
reflect1D k x = if x > k then 2 * k - x else x

solve1 :: [Fold] -> S.Set Point -> S.Set Point
solve1 folds points = foldl applyFold points folds

applyFold :: S.Set Point -> Fold -> S.Set Point
applyFold ps f = S.map (reflect f) ps

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]
