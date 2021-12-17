import Data.Char
import Data.Maybe

data Edge = Edge { getFrom, getTo :: Node } deriving Show
data Node = Start | End | Big String | Small String deriving Eq
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
  print $ solve1 edges start

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

solve1 :: [Edge] -> Path -> Int
solve1 edges (End:_) = 1
solve1 edges path = foldl f 0 nexts
  where f acc n = acc + solve1 edges (n:path)
        nexts = mapMaybe (nextNode path) edges

nextNode :: Path -> Edge -> Maybe Node
nextNode (n:ns) (Edge n1 n2@(Small s)) = if n == n1 && notElem n2 ns then Just n2 else Nothing
nextNode (n:ns) (Edge n1 n2) = if n == n1 then Just n2 else Nothing 
nextNode _       _  = Nothing

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]
