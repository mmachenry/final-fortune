module Main where

import qualified Data.Set as Set
import GameState
import CardDB
import Deck (Deck, readDeck)
import DynamicSearch (bfs)
import System.Environment (getArgs)

main = do
    [filename, gameid] <- getArgs
    deck <- fmap readDeck $ readFile filename
    res <- winningState deck (read gameid)
    print res

winningState :: Deck -> Int -> IO (Maybe GameState)
winningState deck seed =
    bfs
  $ Set.singleton
  $ initGameState seed
  $ map findCard deck
