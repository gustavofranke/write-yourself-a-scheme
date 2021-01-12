module Ch02.RecursiveParser where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main0 :: IO ()
main0 = do args <- getArgs
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
-- >>> readExpr "a"
-- "No match: \"lisp\" (line 1, column 1):\nunexpected \"a\""
-- >>> readExpr "     %"
-- "No match: \"lisp\" (line 1, column 1):\nunexpected \" \""
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

