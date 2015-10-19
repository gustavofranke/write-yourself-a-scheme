module Main where
import System.Environment

-- 1. Change the program so it reads two arguments from the command line,
-- and prints out a message using both of them.
main :: IO ()
main = do args <- getArgs
          putStrLn ("Hello, " ++ args !! 0 ++ " " ++ args !! 1)
