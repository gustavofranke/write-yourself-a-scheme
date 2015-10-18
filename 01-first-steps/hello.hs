module Main where
import System.Environment

main :: IO ()
main = do args <- getArgs
          putStrLn ("Hello, " ++ args !! 0)

-- To compile and run the program, try something like this:
-- >> ghc −o hello_you hello.hs
-- >> ./hello_you Gustavo
-- Hello, Gustavo
