import Data.Bits
import Data.Char
import Data.List

main = do
  strs <- fmap lines getContents
  print $ solve1 strs

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
moreCommonChar cs = boolToChar $ (count (== '1') cs) > (count (== '0') cs)
  where boolToChar b = if b then '1' else '0' 

-- Gets the nth bit of `x`, where the 0th bit is the LSB
getBit :: Int -> Int -> Int
getBit n x = if (nthOne .&. x) > 0 then 1 else 0
  where nthOne = shift 1 n

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred
