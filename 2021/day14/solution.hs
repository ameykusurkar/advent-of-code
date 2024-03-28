import qualified Data.Map as M
import Data.Maybe

main = do
  template <- getLine
  getLine
  rules <- fmap (map parseRule . lines) getContents
  -- print template
  -- print rules
  -- print $ buildCounts 1 (M.fromList rules) "NN"
  print $ solve1 10 (M.fromList rules) template

parseRule :: String -> (String, Char)
parseRule str = (to, head from)
  where [to, "->", from] = words str

solve1 :: Int -> M.Map String Char -> String -> Int
solve1 n rules tmp = maxCount - minCount
  where freq = buildCounts n rules tmp
        maxCount = (maximum . map snd . M.toList) freq
        minCount = (minimum . map snd . M.toList) freq

buildCounts :: Int -> Rules -> String -> Counter Char
buildCounts n rules = theLast . joinMaps . map (step2' n rules . toL) . pairwise
  where toL (a, b) = [a, b]
        step2' n r [a, b] = (a, b, step2 n r [a, b])
        step2' _ _ _ = undefined
        theLast (a, b, c) = c

type Rules = M.Map String Char
type Counter k = M.Map k Int

inc :: Ord k => k -> Counter k -> Counter k
inc k c = M.insert k (v+1) c
  where v = M.findWithDefault 0 k c

dec :: Ord k => k -> Counter k -> Counter k
dec k c = M.insert k (v-1) c
  where v = fromJust (M.lookup k c)

step2 :: Int -> Rules -> String -> Counter Char
step2 0 rules [l, r] = M.fromListWith (+) [(l, 1), (r, 1)]
step2 n rules [l, r] = c''
  where
    c'' = dec m $ M.unionWith (+) c c'
    c = step2 (n-1) rules [l, m]
    c' = step2 (n-1) rules [m, r]
    m = M.findWithDefault '$' [l, r] rules
step2 _ _ _ = undefined

joinMaps :: [(Char, Char, Counter Char)] -> (Char, Char, Counter Char)
joinMaps = foldr1 f
  where f (l, r, c) (l', r', c') = (l, r', M.unionWith (+) (dec r c) c')

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (tail xs)
