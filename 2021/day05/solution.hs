import qualified Data.Map as M
import Data.List

main = do
  segments <- fmap (map readSegment . lines) getContents
  print $ length $ overlap (filter isGridLine segments)
  print $ length $ overlap segments

type Segment = (Point, Point)
type Point = (Int, Int)

readSegment :: String -> Segment
readSegment str = ((read x1, read y1), (read x2, read y2))
  where x1:y1:[] = splitOn ',' p1
        x2:y2:[] = splitOn ',' p2
        p1:_:p2:[] = words str

overlap :: [Segment] -> [Point]
overlap segs = [p | (p, c) <- freqs, c > 1]
  where freqs = (frequency . concatMap points) segs

isGridLine :: Segment -> Bool
isGridLine = or . sequence [horizontal, vertical]

points :: Segment -> [Point]
points seg@((x1, y1), (x2, y2))
  | horizontal seg = zip [minX..maxX] (repeat y2)
  | vertical   seg = zip (repeat x1) [minY..maxY]
  | otherwise      = diagonalPoints seg
  where [minX, maxX] = sort [x1, x2]
        [minY, maxY] = sort [y1, y2]

diagonalPoints :: Segment -> [Point]
diagonalPoints ((x1, y1), (x2, y2)) = zip [minX..maxX] ys
  where [minX, maxX] = sort [x1, x2]
        [minY, maxY] = sort [y1, y2]
        negGrad = (x2 - x1) * (y2 - y1) < 0
        ys = if negGrad then reverse [minY..maxY] else [minY..maxY]

horizontal :: Segment -> Bool
horizontal ((_, y1), (_, y2)) = y1 == y2

vertical :: Segment -> Bool
vertical ((x1, _), (x2, _)) = x1 == x2

frequency :: Ord a => [a] -> [(a, Int)]
frequency xs = M.toList $ M.fromListWith (+) [(x, 1) | x <- xs]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]
