-- {-# LANGUAGE DeriveFoldable, DeriveFunctor, TupleSections #-}

import Data.Char
import Data.List

data Edge = Edge { getFrom, getTo :: Node } deriving Show
data Node = Start | End | Big String | Small String deriving (Eq, Show)
type Path = [Node]

main = do
  edges <- fmap (map parseLine . lines) getContents
  let f = solve1 edges
  print $ [[Start]] >>= f >>= f

parseLine :: String -> Edge
parseLine str = Edge (parseNode n1) (parseNode n2)
  where [n1, n2] = splitOn '-' str
        parseNode "start" = Start
        parseNode "end" = End
        parseNode str = if isUpper (head str) then Big str else Small str

solve1 :: [Edge] -> Path -> [Path]
solve1 = extend

extend :: [Edge] -> Path -> [Path]
extend edges nodes = map (:nodes) (cands edges)
  where cands = map getTo . filter (startsWith (head nodes))
        startsWith n (Edge n1 n2) = n == n1

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]
