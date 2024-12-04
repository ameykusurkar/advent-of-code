import Control.Applicative
import Data.Char (isDigit)
import Control.Monad.Trans.State

main = do
  input <- getContents
  print $ solve input
  print $ solve2 input

solve :: String -> Int
solve str = sum $ filter (> 0) xs
  where
    Just (xs, _) = runStateT extract str

solve2 :: String -> Int
solve2 str = sum $ ignoreDisabled (-1) xs
  where
    Just (xs, _) = runStateT extract str

extract = some $ junk *> (mul <|> doo <|> dont <|> discard) <* junk
  where
    doo = -1 <$ string "do()"
    dont = -2 <$ string "don't()"
    junk = many $ charWhen $ (\c -> not (elem c "dm"))
    discard = 0 <$ charWhen (const True)

mul :: Parser Int
mul = do
  string "mul"
  char '('
  x <- int
  char ','
  y <- int
  char ')'
  return (x * y)

-- Do: -1, Don't: -2
ignoreDisabled _ [] = []
ignoreDisabled _ (-1 : xs) = ignoreDisabled (-1) xs
ignoreDisabled _ (-2 : xs) = ignoreDisabled (-2) xs
ignoreDisabled (-1) (x : xs) = x : ignoreDisabled (-1) xs
ignoreDisabled (-2) (x : xs) = 0 : ignoreDisabled (-2) xs

------ Parser combinator boilerplate -------

type Parser a = StateT String Maybe a

charWhen :: (Char -> Bool) -> Parser Char
charWhen f = StateT p
  where
    p "" = Nothing
    p (c : cs)
      | f c = Just (c, cs)
      | otherwise = Nothing

char :: Char -> Parser Char
char ch = charWhen (ch ==)

int :: Parser Int
int = read <$> digits
  where
    digits = some $ charWhen isDigit

string :: String -> Parser String
string = traverse char
