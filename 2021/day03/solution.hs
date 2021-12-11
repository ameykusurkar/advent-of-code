import Data.Bits
import Data.Char
import Data.List

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
solve2 str = foo * bar
  where foo = parseBinary $ map boolToChar $ winningBa mostCommonBit bas
        bar = parseBinary $ map boolToChar $ winningBa leastCommonBit bas
        bas = transpose $ map (map charToBool) str

charToBool :: Char -> Bool
charToBool = (== '1')

type BitArray = [Bool]

-- Taking an array of bit arrays, returns the bit array where each bit "wins"
-- when comparing against the the other bits at the corresponding position 
winningBa :: (BitArray -> Bool) -> [BitArray] -> BitArray
winningBa _ [] = []
winningBa _ ((b:[]):bas) = b : (map head bas)
winningBa winCond (ba:bas) = winBit : (winningBa winCond winningBas)
  where winningBas = map (filterTrues hasWon) bas
        hasWon = map (== winBit) ba
        winBit = winCond ba

filterTrues :: [Bool] -> [a] -> [a]
filterTrues trues = map snd. filter fst . zip trues

shouldPick :: (a -> Bool) -> [a] -> [Bool]
shouldPick pred xs = map pred xs
