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
-- "No match: \"lisp\" (line 1, column 1):\nunexpected \" \"\nexpecting letter, \"\\\"\" or digit"
-- >>> readExpr "     !"
-- "No match: \"lisp\" (line 1, column 1):\nunexpected \" \"\nexpecting letter, \"\\\"\" or digit"
-- >>> readExpr "     %"
-- "No match: \"lisp\" (line 1, column 1):\nunexpected \" \"\nexpecting letter, \"\\\"\" or digit"
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

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving Show

-- |
-- >>> parse parseString "default" "\"hello\""
-- Right (String "hello")
-- >>> parse parseString "default" "hello"
-- Left "default" (line 1, column 1):
-- unexpected "h"
-- expecting "\""
parseString :: Parser LispVal
parseString = do char '"'
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
parseAtom = do first <- letter <|> symbol
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
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
