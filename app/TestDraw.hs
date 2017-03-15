module Main where

import Deck (readDeck)
import System.Random.Shuffle (shuffleM)
import System.Environment (getArgs)

main :: IO ()
main = do
  [filename] <- getArgs
  deck <- fmap readDeck $ readFile filename
  shuffledCards <- shuffleM deck
  mapM_ putStrLn shuffledCards
  print (length deck)
