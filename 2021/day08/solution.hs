{-# LANGUAGE TupleSections #-}

import Data.List
import Data.Maybe
import qualified Data.Map as M

type Digit = String
type Entry = ([Digit], [Digit])

type Candidates = M.Map Char [Char]
type Solution = M.Map Char Char

main = do
  entries <- fmap (map parseLine . lines) getContents
  print $ solve1 entries
  print $ solve2 entries

parseLine :: String -> Entry
parseLine line = (words signals, words output)
  where [signals, output] = splitOn '|' line

solve1 :: [Entry] -> Int
solve1 =  count (hasLengths [2, 4, 3, 7]) . concatMap snd

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
  (frag, _:rem) -> frag : splitOn c rem
  (frag, [])    -> [frag]

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

hasLengths :: [Int] -> [a] -> Bool
hasLengths lens = or . mapM (==) lens . length

{-
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
-}


panels :: M.Map Digit Char
panels = M.fromList
  [ ("abcefg", '0')
  , ("cf",     '1')
  , ("acdeg",  '2')
  , ("acdfg",  '3')
  , ("bcdf",   '4')
  , ("abdfg",  '5')
  , ("abdefg", '6')
  , ("acf",    '7')
  , ("abcdefg",'8')
  , ("abcdfg", '9') ]

solve2 :: [Entry] -> Int
solve2 = sum . map solveEntry

solveEntry :: Entry -> Int
solveEntry (signals, outputs) = read $ map (decipher solution) sortedPanels
  where solution = deduce signals
        sortedPanels = map sort outputs

deduce :: [Digit] -> Solution
deduce signals = M.fromList [('a', a), ('b', b), ('c', c), ('d', d), ('e', e), ('f', f), ('g', g)]
  where a = head $ acf \\ cf
        b = head $ bcdf \\ [c, d, f]
        c = head $ intersect cf cde
        d = head $ intersect bcdf de
        e = head $ de \\ [d]
        f = head $ cf \\ [c]
        g = head $ allC \\ [a, b, c, d, e, f]
        de = cde \\ [c]
        cde = outliers [snz1, snz2, snz3]
        [cf, acf, bcdf, _, _, _, snz1, snz2, snz3, allC] = sortAll signals

sortAll :: [Digit] -> [Digit]
sortAll signals = map sort (sortBy cLen signals)
  where cLen a b = compare (length a) (length b)

outliers :: [[Char]] -> [Char]
outliers ccs = allC \\ inCommon
  where allC = foldl1 union ccs
        inCommon = foldl1 intersect ccs

decipher :: Solution -> Digit -> Char
decipher solution digit = fromJust $ M.lookup (sort transformed) panels
  where transformed = transform solutionLookup digit
        solutionLookup = (M.fromList . map swap . M.toList) solution
        swap (a, b) = (b, a)

transform :: Solution -> Digit -> Digit
transform sol = map (fromJust . (`M.lookup` sol))
