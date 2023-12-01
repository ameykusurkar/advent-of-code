module Main where

import Data.Char

main = interact $ show . sum . map (solve . unspell) . lines

solve :: String -> Int
solve line = read (first ++ last)
  where first = take 1 digits
        last = take 1 (reverse digits)
        digits = filter isDigit line

unspell []                             = []
unspell ('o':rest@('n':'e':_))         = '1': unspell rest
unspell ('t':rest@('w':'o':_))         = '2': unspell rest
unspell ('t':rest@('h':'r':'e':'e':_)) = '3': unspell rest
unspell ('f':rest@('o':'u':'r':_))     = '4': unspell rest
unspell ('f':rest@('i':'v':'e':_))     = '5': unspell rest
unspell ('s':rest@('i':'x':_))         = '6': unspell rest
unspell ('s':rest@('e':'v':'e':'n':_)) = '7': unspell rest
unspell ('e':rest@('i':'g':'h':'t':_)) = '8': unspell rest
unspell ('n':rest@('i':'n':'e':_))     = '9': unspell rest
unspell (s:rest)                       = s  : unspell rest
