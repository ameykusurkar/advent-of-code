module Main where

import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Monoid

main = do
  seeds <- fmap (ints . drop 7) getLine
  getLine
  maps <- fmap (map parse . filter (not . null ) . splitOn "" . splitOn '\n') getContents
  print $ minimum $ map (follow maps) seeds

ints :: String -> [Int]
ints = map read . words

type Range = [Int]

parse :: [String] -> [Range]
parse = map ints . tail

follow :: [[Range]] -> Int -> Int
follow rss i = transform i $ map fetch rss

transform :: a -> [a -> a] -> a
transform = foldl (\x f -> f x)

fetch :: [Range] -> Int -> Int
fetch rs i = fromMaybe i $ getAlt $ foldMap (Alt . (`rangeLookup` i)) rs

rangeLookup :: Range -> Int -> Maybe Int
rangeLookup [dest, src, width] i
  | src <= i && i < src + width = Just (dest - src + i)
  | otherwise                   = Nothing

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _ : rem) -> frag : splitOn c rem
  (frag, []) -> [frag]
