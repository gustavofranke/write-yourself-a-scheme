module Ch02.SimpleParser2 where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main0 :: IO ()
main0 = do args <- getArgs
           putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+_/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

-- ghc -package parsec -o ../bin/simple_parser simple-parser2.hs 
-- ../bin/simple_parser "    %"
-- Found value
-- ../bin/simple_parser %
-- No match: "lisp" (line 1, column 1):
-- unexpected "%"
-- expecting space
-- ../bin/simple_parser "    asd"
-- No match: "lisp" (line 1, column 5):
-- unexpected "a"
-- expecting space
