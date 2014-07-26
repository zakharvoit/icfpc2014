module MonadicParser where


data Lexem = Str String | LNumber Int | LName String | LList [Lexem]
           deriving Show

type Error = String

newtype Lexer c a = Lexer {
  runLex :: [c] -> Either ([c], Error) ([c], a)
}

instance Monad (Lexer c) where
  return a = Lexer $ \s -> Right (s, a)
  ma >>= f = Lexer $ \s -> case runLex ma s of
                           Left x -> Left x
                           Right (s', a) -> runLex (f a) s'

char :: (Show c, Eq c) => c -> Lexer c c
char x = like (== x) $ show x

anyOf :: (Show c, Eq c) => [c] -> Lexer c c
anyOf s = like (`elem` s) $ "something in " ++ show s

space :: Lexer Char Char
space = anyOf " \t\n"
spaces :: Lexer Char String
spaces = many space
lexem :: Lexer Char a -> Lexer Char a
lexem p = do
  x <- p
  spaces
  return x

noneOf :: (Show c, Eq c) => [c] -> Lexer c c
noneOf s = like (not . (`elem` s)) $ "something not in " ++ show s

many1 :: Lexer c a -> Lexer c [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

many :: Lexer c a -> Lexer c [a]
many p = many1 p <|> return []

like :: Show c => (c -> Bool) -> Error -> Lexer c c
like p err = do
  c <- anychar
  if p c
     then return c
     else oops $ "expected " ++ err ++ " got " ++ show c

oops :: String -> Lexer c a
oops desc = Lexer$ \s -> Left (s, desc)

anychar :: Lexer c c
anychar = Lexer $ \s -> case s of
  [] -> Left ([], "eof")
  (a:as) -> Right (as, a)

try :: Lexer c a -> Lexer c (Maybe a)
try p = Lexer $ \s -> case runLex p s of
  Left (s', e) -> Right (s, Nothing)
  Right (s', a) -> Right (s', Just a)

(<|>) :: Lexer c a -> Lexer c a -> Lexer c a
p <|> s = do
  x <- try p
  case x of
    Just a -> return a
    Nothing -> s
