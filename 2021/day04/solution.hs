import Data.Maybe
import Data.List

main = do
  nums <- fmap parseNums getLine
  getLine
  boards <- fmap parseBoards getContents
  print $ solve nums boards

parseNums:: String -> [Int]
parseNums = map read . splitOn ','

type Board = [[(Int, Bool)]]

parseBoards :: String -> [Board]
parseBoards = map buildBoard . splitOn "" . lines
  where buildBoard = map (map (\x -> (x, False))) . map (map read) . map words

solve :: [Int] -> [Board] -> Int
solve (n:ns) boards = case find didWin boards' of
  Just board -> n * sum (unmarked board)
  Nothing -> solve ns boards'
  where boards' = map (bingo n) boards

didWin :: Board -> Bool
didWin rows = any allTrue rows || any allTrue (transpose rows)
  where allTrue = all id . map snd

unmarked :: Board -> [Int]
unmarked = map fst. filter (not . snd) . concat

bingo :: Int -> Board -> Board
bingo n rows = map updateRow rows
  where updateRow = map (\(x, b) -> if n == x then (x, True) else (x, b))

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]
