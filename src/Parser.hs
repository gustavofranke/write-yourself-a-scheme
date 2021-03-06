{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Data.Text as T
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
-- >>> parse parseExpr "default" "-2"
-- Right -2
-- >>> parse parseExpr "default" "2"
-- Right 2
-- >>> parse parseExpr "default" "\"a string\""
-- Right "a string'
-- >>> parse parseExpr "default" "(+ 2 2)"
-- Right (+ 2 2)
-- >>> parse parseExpr "default" "(1 2 3)"
-- Right (1 2 3)
-- >>> parse parseExpr "default" "'(1 2 3)"
-- Right (quote (1 2 3))
-- >>> parse parseExpr "default" "\"(if (> 2 3) \"no\" \"yes\")\""
-- Right "(if (> 2 3) '
parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      _ <- char '('
      x <- try parseList <|> parseDottedList
      _ <- char ')'
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
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String $ T.pack x

-- |
-- >>> parse parseAtom "default" "hello"
-- Right hello
-- >>> parse parseAtom "default" "he7l8lo"
-- Right he7l8lo
-- >>> parse parseAtom "default" "he7l8lo#t"
-- Right he7l8lo#t
-- >>> parse parseAtom "default" "#t"
-- Right #t
-- >>> parse parseAtom "default" "-123"
-- Right -123
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
    _ -> Atom $ T.pack atom

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
-- >>> parse parseList "default" "(1 2 3)"
-- Right ((1 2 3))
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head0 <- endBy parseExpr spaces
  tail0 <- char '.' >> spaces >> parseExpr
  return $ DottedList head0 tail0

-- |
-- >>> parse parseQuoted "default" "'456"
-- Right (quote 456)
-- >>> parse parseQuoted "default" "456"
-- Left "default" (line 1, column 1):
-- unexpected "4"
-- expecting "'"
parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
