{-# LANGUAGE OverloadedStrings #-}

module Exercises where

import Data.Functor
import qualified Data.Text as T
import LispVal
import Text.ParserCombinators.Parsec

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
parseNumber1 = many1 digit <&> Number . read

-- 2. Our strings aren't quite R5RS compliant,
-- because they don't support escaping of internal quotes within the string.
-- Change parseString so that \" gives a literal quote character instead of terminating the string.
-- You may want to replace noneOf "\"" with a new parser action that accepts
-- either a non-quote character or a backslash followed by a quote mark.
-- |
-- >>> parse parseString0 "default" "\"hello\""
-- Right "hello'
-- >>> parse parseString0 "default" "\"hello \"how\" are you\""
-- Right "hello '
parseString0 :: Parser LispVal
parseString0 = do _ <- char '"'
                  x <- many (noneOf "\"" <|> do char '\\' >> char '"')
                  _ <- char '"'
                  return $ String (T.pack x)
-- 3. Modify the previous exercise to support \n, \r, \t, \\, and any other desired escape characters.

-- |
-- >>> parse parseString1 "default" "\"hello\""
-- Right "hello'
-- >>> parse parseString1 "default" "\"hello\nhello\""
-- Right "hello
-- hello'
-- >>> parse parseString1 "default" "\"hello\thello\""
-- Right "hello	hello'
-- >>> parse parseString1 "default" "\"hello\\hello\""
-- Right "hello\hello'
parseString1 :: Parser LispVal
parseString1 = do
  _ <- char '"'
  x <- many (noneOf "\"" <|> oneOf "\n\r\t\\")
  _ <- char '"'
  return $ String (T.pack x)

-- 4. Change parseNumber to support the Scheme standard for different bases.
-- You may find the readOct and readHex functions useful.

-- 5. Add a Character constructor to LispVal,
-- and create a parser for character literals as described in R5RS.

data LispVal2
  = Atom T.Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Bool Bool
  | Charac Char deriving Show

-- |
-- >>> parse parseChar "charparser" "a"
-- Right (Charac 'a')
-- >>> parse parseChar "charparser" "\n"
-- Right (Charac '\n')
parseChar :: Parser LispVal2
parseChar = Charac <$> anyChar

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
-----------------------------------------------------------------------------
-- Exercises
-- 1. Add primitives to perform the various type-testing functions of R5RS:
-- symbol?, string?, number?, etc.

-- 2. Change unpackNum so that it always returns 0 if the value is not a number,
-- even if it's a string or list that could be parsed as a number.

-- 3. Add the symbol-handling functions from R5RS.
-- A symbol is what we've been calling an Atom in our data constructors.
-----------------------------------------------------------------------------
-- Exercises
-- 1. Instead of treating any non-false value as true,
-- change the definition of if so that the predicate accepts only Bool values and throws an error on any others.

-- 2. equal? has a bug in that a list of values is compared using eqv? instead of equal?.
-- For example, (equal? '(1 "2") '(1 2)) = #f, while you'd expect it to be true.
-- Change equal? so that it continues to ignore types as it recurses into list structures.
-- You can either do this explicitly, following the example in eqv?,
-- or factor the list clause into a separate helper function that is parameterized by the equality testing function.

-- 3. Implement cond and case expressions.

-- 4. Add the rest of the string functions.
-- You don't yet know enough to do string-set!;
-- this is difficult to implement in Haskell, but you'll have enough information after the next two sections.
-----------------------------------------------------------------------------
-- Exercises
-- 1. Instead of treating any non-false value as true, change the definition of if
-- so that the predicate accepts only Bool values and throws an error on any others.

-- 2. equal? has a bug in that a list of values is compared using eqv? instead of equal?.
-- For example, (equal? '(1 "2") '(1 2)) = #f, while you'd expect it to be true.
-- Change equal? so that it continues to ignore types as it recurses into list structures.
-- You can either do this explicitly, following the example in eqv?,
-- or factor the list clause into a separate helper function that is parameterized by the equality testing function.

-- 3. Implement cond and case expressions.

-- 4. Add the rest of the string functions.
-- You don't yet know enough to do string-set!; this is difficult to implement in Haskell,
-- but you'll have enough information after the next two sections.
