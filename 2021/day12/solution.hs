{-# LANGUAGE TupleSections #-}

import Data.Char
import Data.Maybe
import qualified Data.Map as M

data Edge = Edge { getFrom, getTo :: Node } deriving Show
data Node = Start | End | Big String | Small String deriving (Ord, Eq)
type Path = [Node]

instance Show Node where
  show Start = "start"
  show End = "end"
  show (Big str) = str
  show (Small str) = str

main = do
  rawEdges <- fmap (map parseLine . lines) getContents
  let edges = concatMap buildEdges rawEdges
  let start = [Start]
  print $ solve allowSmallOnce edges start
  print $ solve allowOneSmallTwice edges start

buildEdges :: Edge -> [Edge]
buildEdges e@(Edge Start _) = [e]
buildEdges e@(Edge n Start) = [Edge Start n]
buildEdges e@(Edge _ End) = [e]
buildEdges (Edge n1 n2) = [Edge n1 n2, Edge n2 n1]

parseLine :: String -> Edge
parseLine str = Edge (parseNode n1) (parseNode n2)
  where [n1, n2] = splitOn '-' str
        parseNode "start" = Start
        parseNode "end" = End
        parseNode str = if isUpper (head str) then Big str else Small str

type SmallGuard = Node -> Path -> Bool

solve :: SmallGuard -> [Edge] -> Path -> Int
solve sg edges (End:_) = 1
solve sg edges path = foldl f 0 nexts
  where f acc n = acc + solve sg edges (n:path)
        nexts = map getTo $ filter (canExtend sg path) edges

canExtend :: SmallGuard -> Path -> Edge -> Bool
canExtend sg (n:ns) (Edge n1 n2@(Small s)) = n == n1 && sg n2 (n:ns)
canExtend _  (n:ns) (Edge n1 n2)           = n == n1
canExtend _  _       _                     = False

allowSmallOnce :: SmallGuard
allowSmallOnce = notElem

allowOneSmallTwice :: SmallGuard
allowOneSmallTwice sml path = count sml == 0 || null twos
  where count s = M.findWithDefault 0 s m
        twos = filter (\s -> count s == 2) smalls
        m = buildCounter smalls
        smalls = filter isSmall path
        isSmall (Small _) = True
        isSmall _         = False

buildCounter :: Ord a => [a] -> M.Map a Int
buildCounter = M.fromListWith (+) . map (,1)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]
