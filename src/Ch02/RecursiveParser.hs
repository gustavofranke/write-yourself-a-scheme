module Ch02.RecursiveParser where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main0 :: IO ()
main0 = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))

-- |
-- >>> parse symbol "" "$"
-- Right '$'
-- >>> parse symbol "" "a"
-- Left (line 1, column 1):
-- unexpected "a"
symbol :: Parser Char
symbol = oneOf "!$%&|*+_/:<=?>@^_~#"

-- |
-- >>> readExpr "$"
-- "Found value"
-- >>> readExpr "     $"
-- "No match: \"lisp\" (line 1, column 1):\nunexpected \" \"\nexpecting letter, \"\\\"\", digit, \"'\" or \"(\""
-- >>> readExpr "     !"
-- "No match: \"lisp\" (line 1, column 1):\nunexpected \" \"\nexpecting letter, \"\\\"\", digit, \"'\" or \"(\""
-- >>> readExpr "     %"
-- "No match: \"lisp\" (line 1, column 1):\nunexpected \" \"\nexpecting letter, \"\\\"\", digit, \"'\" or \"(\""
-- >>> readExpr "(a test)"
-- "Found value"
-- >>> readExpr "(a (nested) test)"
-- "Found value"
-- >>> readExpr "(a (dotted . list) test)"
-- "Found value"
-- >>> readExpr "(a '(quoted (dotted . list)) test)"
-- "Found value"
-- >>> readExpr "(a '(imbalanced parens)"
-- "No match: \"lisp\" (line 1, column 24):\nunexpected end of input\nexpecting space or \")\""
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

-- |
-- >>> parse spaces "default" "    sd"
-- Right ()
-- >>> parse spaces "default" "sd"
-- Left "default" (line 1, column 1):
-- unexpected "s"
-- expecting space
spaces :: Parser ()
spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show)

-- |
-- >>> parse parseString "default" "\"hello\""
-- Right (String "hello")
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
-- Right (Atom "hello")
-- >>> parse parseAtom "default" "he7l8lo"
-- Right (Atom "he7l8lo")
-- >>> parse parseAtom "default" "he7l8lo#t"
-- Right (Atom "he7l8lo#t")
-- >>> parse parseAtom "default" "#t"
-- Right (Bool True)
-- >>> parse parseAtom "default" "123"
-- Left "default" (line 1, column 1):
-- unexpected "1"
-- expecting letter
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = [first] ++ rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    otherwise -> Atom atom

-- |
-- >>> parse parseNumber "default" "123"
-- Right (Number 123)
-- >>> parse parseNumber "default" "123asd"
-- Right (Number 123)
-- >>> parse parseNumber "default" "asd123asd"
-- Left "default" (line 1, column 1):
-- unexpected "a"
-- expecting digit
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- |
-- >>> parse parseExpr "default" "\"hello\""
-- Right (String "hello")
-- >>> parse parseExpr "default" "hello"
-- Right (Atom "hello")
-- >>> parse parseExpr "default" "123"
-- Right (Number 123)
-- >>> parse parseExpr "default" "true"
-- Right (Atom "true")
parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- (try parseList) <|> parseDottedList
      char ')'
      return x

-----------------------------------------------------------------------------
-- Exercises
-- 1. Rewrite parseNumber using
-- (a) do-notation.
parseNumber0 :: Parser LispVal
parseNumber0 = do
  ds <- many1 digit
  pure $ (Number . read) ds

-- (b) explicit sequencing with the >>= operator.
parseNumber1 :: Parser LispVal
parseNumber1 = many1 digit >>= pure . Number . read

-- 2. Our strings aren’t quite R5RS compliant,
-- because they don’t support escaping of internal quotes within the string.
-- Change parseString so that \" gives a literal quote character instead of terminating the string.
-- You may want to replace noneOf "\"" with a new parser action that accepts
-- either a non-quote character or a backslash followed by a quote mark.

-- 3. Modify the previous exercise to support \n, \r, \t, \\, and any other desired escape characters.

-- 4. Change parseNumber to support the Scheme standard for different bases.
-- You may find the readOct and readHex functions useful.

-- 5. Add a Character constructor to LispVal,
-- and create a parser for character literals as described in R5RS.

-- 6. Add a Float constructor to LispVal, and support R5RS syntax for decimals.
-- The Haskell function readFloat may be useful.

-- 7. Add data types and parsers to support the full numeric tower of Scheme numeric types.
-- Haskell has built-in types to represent many of these; check the Prelude.
-- For the others, you can define compound types that represent
-- eg. a Rational as a numerator and denominator,
-- or a Complex as a real and imaginary part (each itself a Real number).
-----------------------------------------------------------------------------
-- Exercises
-- 1. Add support for the backquote syntactic sugar:
-- the Scheme standard details what it should expand into (quasiquote/unquote).

-- 2. Add support for vectors.
-- The Haskell representation is up to you:
-- GHC does have an Array data type, but it can be difficult to use.
-- Strictly speaking, a vector should have constant-time indexing and updating,
-- but destructive update in a purely functional language is difficult.
-- You may have a better idea how to do this after the section on set!,
-- later in this tutorial.

-- 3. Instead of using the try combinator,
-- left-factor the grammar so that the common subsequence is its own parser.
-- You should end up with a parser that matches a string of expressions,
-- and one that matches either nothing or a dot and a single expressions.
-- Combining the return values of these into either a List or a DottedList is left
-- as a (somewhat tricky) exercise for the reader:
-- you may want to break it out into another helper function.
