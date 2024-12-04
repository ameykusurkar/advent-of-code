module Main where

import Data.List (transpose)

main = do
  raw <- getContents
  let parsed = lines raw
  print $ solve parsed
  print $ solve2 parsed

solve :: [[Char]] -> Int
solve xs = diagC + horizC + vertC + diag2C
  where
    count ys = sum $ map xmas ys
    diagC = count $ diagonals xs
    diag2C = count $ diagonals $ map reverse xs
    horizC = count xs
    vertC = count $ transpose xs

diagonals xs = topDiagonals xs ++ drop 1 (topDiagonals (transpose xs))

solve2 :: [[Char]] -> Int
solve2 as = length $ filter id $ do
  x <- [0 .. length (head as)]
  y <- [0 .. length as]
  return $ crossmas $ square3 x y as

xmas :: String -> Int
xmas ('X' : 'M' : 'A' : 'S' : xs) = 1 + xmas ('S' : xs)
xmas ('S' : 'A' : 'M' : 'X' : xs) = 1 + xmas ('X' : xs)
xmas (_ : xs) = xmas xs
xmas [] = 0

square3 :: Int -> Int -> [[a]] -> [[a]]
square3 x y as = take 3 $ drop y restX
  where
    restX = map (take 3 . drop x) as

crossmas :: [[Char]] -> Bool
crossmas as = forward && backward
  where
    forward = mas (diag as)
    backward = mas $ diag $ map reverse as
    mas x = (x == "MAS") || (x == "SAM")

mas :: String -> Int
mas ('M' : 'A' : 'S' : xs) = 1 + xmas ('S' : xs)
mas ('S' : 'A' : 'M' : 'X' : xs) = 1 + xmas ('X' : xs)
mas (x : xs) = xmas xs
mas [] = 0

topDiagonals :: (Show a) => [[a]] -> [[a]]
topDiagonals [] = []
topDiagonals xs = diag xs : topDiagonals (topSquare xs)

diag :: (Show a) => [[a]] -> [a]
diag [] = []
diag ([] : _) = []
diag ((x : _) : ys) = x : diag rest
  where
    rest = map (drop 1) ys

topSquare :: (Show a) => [[a]] -> [[a]]
topSquare [] = []
topSquare ([] : _) = []
topSquare ([_] : _) = []
topSquare ys = map (drop 1) ys
