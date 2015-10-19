module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+_/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

-- To compile and run this
-- ghc -package parsec -o ../bin/simple_parser simple-parser1.hs
-- usage: ../bin/simple_parser $
-- returns: Found value
-- usage: ../bin/simple_parser a
-- returns: No match: "lisp" (line 1, column 1):
--         unexpected "a"

