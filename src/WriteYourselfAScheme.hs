module WriteYourselfAScheme where

import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

-- |
-- >>> parse symbol "" "$"
-- Right '$'
-- >>> parse symbol "" "a"
-- Left (line 1, column 1):
-- unexpected "a"
symbol :: Parser Char
symbol = oneOf "!$%&|*+_/:<=?>@^_~#"

-- |
-- >>> readExpr "     $"
-- Left Parse error at "lisp" (line 1, column 1):
-- unexpected " "
-- expecting letter, "\"", digit, "'" or "("
-- >>> readExpr "     !"
-- Left Parse error at "lisp" (line 1, column 1):
-- unexpected " "
-- expecting letter, "\"", digit, "'" or "("
-- >>> readExpr "     %"
-- Left Parse error at "lisp" (line 1, column 1):
-- unexpected " "
-- expecting letter, "\"", digit, "'" or "("
-- >>> readExpr "(a test)"
-- Right (a test)
-- >>> readExpr "(a (nested) test)"
-- Right (a (nested) test)
-- >>> readExpr "(a (dotted . list) test)"
-- Right (a (dotted.list) test)
-- >>> readExpr "(a '(quoted (dotted . list)) test)"
-- Right (a (quote (quoted (dotted.list))) test)
-- >>> readExpr "(a '(imbalanced parens)"
-- Left Parse error at "lisp" (line 1, column 24):
-- unexpected end of input
-- expecting space or ")"
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

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
  let atom = [first] ++ rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    otherwise -> Atom atom

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
      x <- (try parseList) <|> parseDottedList
      char ')'
      return x

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\'"
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive functions args" func)
    ($ args)
    $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("reminder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=))
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected
    ++ " args: found values "
    ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

-- instance Except LispError where
-- noMsg = Default "An error has occurred"
-- strMsg = Default

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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
-----------------------------------------------------------------------------
-- Exercises
-- 1. Add primitives to perform the various type-testing functions of R5RS:
-- symbol?, string?, number?, etc.

-- 2. Change unpackNum so that it always returns 0 if the value is not a number,
-- even if it’s a string or list that could be parsed as a number.

-- 3. Add the symbol-handling functions from R5RS.
-- A symbol is what we’ve been calling an Atom in our data constructors.
-----------------------------------------------------------------------------
-- Exercises
-- 1. Instead of treating any non-false value as true,
-- change the definition of if so that the predicate accepts only Bool values and throws an error on any others.

-- 2. equal? has a bug in that a list of values is compared using eqv? instead of equal?.
-- For example, (equal? ’(1 "2") ’(1 2)) = #f, while you’d expect it to be true.
-- Change equal? so that it continues to ignore types as it recurses into list structures.
-- You can either do this explicitly, following the example in eqv?,
-- or factor the list clause into a separate helper function that is parameterized by the equality testing function.

-- 3. Implement cond and case expressions.

-- 4. Add the rest of the string functions.
-- You don’t yet know enough to do string-set!;
-- this is difficult to implement in Haskell, but you’ll have enough information after the next two sections.