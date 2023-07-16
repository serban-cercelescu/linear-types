module Main (main) where

import Prelude ((<$>))
import Prelude.Linear
import Sieve


main :: IO ()
main = do
    putStrLn "Enter a number:"
    n <- read <$> getLine
    putStrLn "The primes smaller than your number are:"
    print $ sieve n
