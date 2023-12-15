module Main where

import Data.Char (ord, digitToInt)

main = do
  seq <- fmap (splitOn ',') getLine
  print $ sum $ map hash seq
  let boxes = map (,[]) [0..255]
  print $ sum $ map collect $ foldl step boxes seq 

collect :: Box -> Int
collect (i, ls) = sum $ zipWith f [1..] ls
  where f n (lbl, fcl) = (i + 1) * n * fcl

type Box = (Int, [Lens])
type Lens = (String, Int)

step :: [Box] -> String -> [Box]
step bs s = apply bs lbl b op n
  where
    (lbl, b, op, n) = parseInst s

parseInst :: String -> (String, Int, Char, Int)
parseInst s
  | last s == '-' = (takeWhile (/='-') s, hash (takeWhile (/='-') s), '-', 0)
  | otherwise     = (takeWhile (/='=') s, hash (takeWhile (/='=') s), '=', digitToInt (last s))

apply :: [Box] -> String -> Int -> Char -> Int -> [Box]
apply ((bn,bs):bxs) lbl b op n
  | bn == b   = (bn, modifyBox bs lbl op n) : bxs
  | otherwise = (bn, bs) : apply bxs lbl b op n

modifyBox :: [Lens] -> String -> Char -> Int -> [Lens]
modifyBox []           lbl '=' n = [(lbl, n)]
modifyBox []           lbl '-' _ = []
modifyBox ((l,val):ls) lbl op  n
  | l == lbl && op == '=' = (l,n) : ls
  | l == lbl && op == '-' = ls
  | otherwise             = (l, val) : modifyBox ls lbl op n

hash :: String -> Int
hash = foldl f 0
  where f val = (`mod` 256) . (*17) . (+val) . ord

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _ : rem) -> frag : splitOn c rem
  (frag, []) -> [frag]
