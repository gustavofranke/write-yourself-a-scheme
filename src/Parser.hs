module Parser where

import LispVal
import Text.ParserCombinators.Parsec hiding (spaces)

-- |
-- >>> parse spaces "default" "    sd"
-- Right ()
-- >>> parse spaces "default" "sd"
-- Left "default" (line 1, column 1):
-- unexpected "s"
-- expecting space
spaces :: Parser ()
spaces = skipMany1 space

-- |
-- >>> parse symbol "" "$"
-- Right '$'
-- >>> parse symbol "" "a"
-- Left (line 1, column 1):
-- unexpected "a"
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

-- |
-- >>> parse parseExpr "default" "\"hello\""
-- Right "hello'
-- >>> parse parseExpr "default" "hello"
-- Right hello
-- >>> parse parseExpr "default" "123"
-- Right 123
-- >>> parse parseExpr "default" "true"
-- Right true
-- >>> parse parseExpr "default" "(1 2 2)"
-- Right (1 2 2)
-- >>> parse parseExpr "default" "'(1 3 (\"this\" \"one\"))"
-- Right (quote (1 3 ("this' "one')))
-- >>> parse parseExpr "default" "'atom"
-- Right (quote atom)
-- >>> parse parseExpr "default" "2"
-- Right 2
-- >>> parse parseExpr "default" "\"a string\""
-- Right "a string'
-- >>> parse parseExpr "default" "(+ 2 2)"
-- Right (+ 2 2)
parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

-- |
-- >>> parse parseString "default" "\"hello\""
-- Right "hello'
-- >>> parse parseString "default" "hello"
-- Left "default" (line 1, column 1):
-- unexpected "h"
-- expecting "\""
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

-- |
-- >>> parse parseAtom "default" "hello"
-- Right hello
-- >>> parse parseAtom "default" "he7l8lo"
-- Right he7l8lo
-- >>> parse parseAtom "default" "he7l8lo#t"
-- Right he7l8lo#t
-- >>> parse parseAtom "default" "#t"
-- Right #t
-- >>> parse parseAtom "default" "123"
-- Left "default" (line 1, column 1):
-- unexpected "1"
-- expecting letter
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

-- |
-- >>> parse parseNumber "default" "123"
-- Right 123
-- >>> parse parseNumber "default" "123asd"
-- Right 123
-- >>> parse parseNumber "default" "asd123asd"
-- Left "default" (line 1, column 1):
-- unexpected "a"
-- expecting digit
parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

-- |
-- >>> parse parseList "default" "456"
-- Right (456)
-- >>> parse parseList "default" "asdf"
-- Right (asdf)
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- |
-- >>> parse parseQuoted "default" "'456"
-- Right (quote 456)
-- >>> parse parseQuoted "default" "456"
-- Left "default" (line 1, column 1):
-- unexpected "4"
-- expecting "'"
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
