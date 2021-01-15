module Main where

import System.Environment
import WriteYourselfAScheme
import Control.Monad (liftM)

main :: IO ()
main = do
    args   <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
