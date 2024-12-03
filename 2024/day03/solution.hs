import Control.Applicative
import Data.Char (isDigit)

main = do
  input <- getContents
  print $ solve input
  print $ solve2 input

solve :: String -> Int
solve str = sum $ filter (> 0) xs
  where
    Just (xs, _) = parse extract str

solve2 :: String -> Int
solve2 str = sum $ ignoreDisabled (-1) xs
  where
    Just (xs, _) = parse extract str

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

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f p = do
    x <- p
    return (f x)

instance Applicative Parser where
  pure x = Parser (\input -> Just (x, input))
  pf <*> pa = do
    f <- pf
    a <- pa
    return (f a)

instance Monad Parser where
  Parser pa >>= f = Parser pb
    where
      pb str = do
        (a, str') <- pa str
        parse (f a) str'

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p) <|> (Parser q) = Parser (\input -> p input <|> q input)

charWhen :: (Char -> Bool) -> Parser Char
charWhen f = Parser p
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
