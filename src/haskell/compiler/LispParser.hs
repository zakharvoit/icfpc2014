module LispParser where

import MonadicParser

exprParser :: Lexer Char Lexem
exprParser = do
  l <- lexem $ listParser <|>  atomParser
  return l

atomParser :: Lexer Char Lexem
atomParser = do
  l <- lexem $ numParser  <|> strParser <|> nameParser
  return l

listParser :: Lexer Char Lexem
listParser = do
  lexem $ char '('
  l <- many $ lexem exprParser
  lexem $ char ')'
  return $ LList l

numParser :: Lexer Char Lexem
numParser = do
  i <- many1 (anyOf "0123456789")
  return $ LNumber $ read i


nameParser:: Lexer Char Lexem
nameParser = do
  l <- many1 (noneOf " \t\n()")
  return $ LName l


strParser :: Lexer Char Lexem
strParser = do
  char '"'
  l <- many (noneOf ['"'])
  char '"'
  return $ Str l

runParse :: Lexer Char [Lexem] -> [Char] -> [Lexem]
runParse p s = case runLex p s of
  Left  (_, e) -> error "Parse error"
  Right ("", a) -> a
  Right (as, a) -> error "Parse error"

parser :: Lexer Char [Lexem]
parser = many $ lexem $ exprParser

lexer :: [Char] -> [Lexem]
lexer s = runParse parser s'
  where
    s' = unlines $ filter (/= "") $ map (takeWhile (/= ';')) $ lines s
