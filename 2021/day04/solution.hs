import Data.List

main = do
  nums <- fmap parseNums getLine
  getLine
  boards <- fmap parseBoards getContents
  print $ nthWin 1 nums boards
  print $ nthWin (length boards) nums boards

parseNums:: String -> [Int]
parseNums = map read . splitOn ','

type Board = [[(Int, Bool)]]

parseBoards :: String -> [Board]
parseBoards = map buildBoard . splitOn "" . lines
  where buildBoard = map (map buildCell . words)
        buildCell x = (read x, False)

nthWin :: Int -> [Int] -> [Board] -> Int
nthWin w [] _ = error (show w)
nthWin w (n:ns) boards = case (w, piles didWin boards') of
  (1, (board:_, _)) -> n * sum (unmarked board)
  (_, (wins, remaining)) -> nthWin (w - length wins) ns remaining
  where boards' = map (bingo n) boards

didWin :: Board -> Bool
didWin rows = any allTrue rows || any allTrue (transpose rows)
  where allTrue = all snd

unmarked :: Board -> [Int]
unmarked = map fst. filter (not . snd) . concat

bingo :: Int -> Board -> Board
bingo n = (map . map) update
  where update (x, b) = (x, n == x || b)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]

piles :: (a -> Bool) -> [a] -> ([a], [a])
piles pred = foldl addToPile ([], [])
  where addToPile (ts, fs) x = if pred x then (x:ts, fs) else (ts, x:fs)
