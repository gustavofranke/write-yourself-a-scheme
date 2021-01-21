module Main where

import System.Environment
import WriteYourselfAScheme
import Control.Monad (liftM)

main :: IO ()
main = do
    args   <- getArgs
    if null args then runRepl else runOne $ args
