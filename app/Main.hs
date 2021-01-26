module Main where

import Repl ( runRepl, runOne )
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args
