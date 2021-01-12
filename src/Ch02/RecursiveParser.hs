module Ch02.RecursiveParser where

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
-- >>> readExpr "     $"
-- "Found value"
-- >>> readExpr "     !"
-- "Found value"
-- >>> readExpr "     %"
-- "Found value"
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
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

-- To compile and run this
-- ghc -package parsec -o ../bin/simple_parser simple-parser1.hs
-- usage: ../bin/simple_parser $
-- returns: Found value
-- usage: ../bin/simple_parser a
-- returns: No match: "lisp" (line 1, column 1):
--         unexpected "a"
