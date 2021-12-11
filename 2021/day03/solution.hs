import Data.Bits
import Data.Char
import Data.List

-- TODO: Needs cleaning up

main = do
  strs <- fmap lines getContents
  print $ solve1 strs
  print $ solve2 strs

parseBinary :: String -> Int
parseBinary str = sum $ zipWith shift bits indexes
  where bits = map digitToInt str
        indexes = reverse $ take (length str) [0..]

solve1 :: [String] -> Int
solve1 strs = (mostCommonBits bits) * (leastCommonBits bits)
 where
   mostCommonBits = parseBinary . map moreCommonChar
   leastCommonBits = parseBinary . map (flipC . moreCommonChar)
   bits = transpose strs

flipC :: Char -> Char
flipC = intToDigit . (1-) . fromEnum . (== '1')

moreCommonChar :: [Char] -> Char
moreCommonChar = boolToChar . mostCommonBit . map (== '1')

boolToChar :: Bool -> Char
boolToChar b = if b then '1' else '0'

mostCommonBit :: [Bool] -> Bool
mostCommonBit bs = (count id bs) >= (count (not . id) bs)

leastCommonBit :: [Bool] -> Bool
leastCommonBit = not . mostCommonBit

-- Gets the nth bit of `x`, where the 0th bit is the LSB
getBit :: Int -> Int -> Int
getBit n x = if (nthOne .&. x) > 0 then 1 else 0
  where nthOne = shift 1 n

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

solve2 :: [String] -> Int
-- solve2 str = foo * bar
solve2 str = (winBy isLeastCommonBit bas) * (winBy isMostCommonBit bas)
  where winBy f = parseBinary . findWinner f
        mostCommonBit bs = (count (== '1') bs) >= (count (== '0') bs)
        leastCommonBit = boolToChar . not . mostCommonBit
        isMostCommonBit ba = map (== ((boolToChar . mostCommonBit) ba)) ba
        isLeastCommonBit ba = map (== (leastCommonBit ba)) ba
        bas = transpose str

-- Takes an array of "rounds" where each round is an array of candidates.
-- Candidates at the same index across rounds are part of the same team.
-- A candidate "wins" if it is in the majority in that round, and the team
-- progresses to the next round. Returns the "winning" team.
findWinner :: ([a] -> [Bool]) -> [[a]] -> [a]
findWinner _didWin [] = []
findWinner _didWin ((cand:[]):rounds) = cand : (map head rounds)
findWinner didWin (round:rounds) = winner : (findWinner didWin winningTeams)
  where winningTeams = map (filterTrues hasWon) rounds
        hasWon = didWin round
        winner = head $ filterTrues hasWon round 

filterTrues :: [Bool] -> [a] -> [a]
filterTrues trues = map snd. filter fst . zip trues
