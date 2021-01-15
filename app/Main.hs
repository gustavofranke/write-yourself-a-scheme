module Main where

import System.Environment
import WriteYourselfAScheme

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
