module Ch01.Hello where

import System.Environment

main0 :: IO ()
main0 = do
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0)

-- To compile and run the program, try something like this:
-- >> ghc -o hello_you hello.hs
-- >> ./hello_you Gustavo
-- Hello, Gustavo

-- Exercises

-- | 1. Change the program so it reads two arguments from the command line,
-- and prints out a message using both of them.
ex1 :: IO ()
ex1 = do
  args <- getArgs
  putStrLn ("Hello, " ++ head args ++ " and " ++ args !! 1)

-- | 2. Change the program so it performs a simple arithmetic
-- operation on the two arguments and prints out the result.
-- You can use read to convert a string to a number, and show to convert a number back into a string.
-- Play around with different operations.
ex2 :: IO ()
ex2 = do
  args <- getArgs
  let a = read (head args) :: Int
      b = read (args !! 1) :: Int
  putStrLn ("Hello, " ++ show (a * b))

-- | 3. getLine is an IO action that reads a line from the console and returns it as a string.
-- Change the program so it prompts for a name, reads the name,
-- and then prints that instead of the command line value.
ex3 :: IO ()
ex3 = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name)
