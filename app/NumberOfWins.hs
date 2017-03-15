module Main where

import Deck (readDeck)
import DynamicSearch (numberOfWins)
import System.Environment (getArgs)

main = do
    [filename, trials] <- getArgs
    deck <- fmap readDeck $ readFile filename
    putStrLn $ show $ numberOfWins deck (read trials)
