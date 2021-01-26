module Main where

import Repl ( runRepl, runOne )
import System.Environment ( getArgs )
import qualified Data.Text as T
main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne (fmap T.pack args)
