module Main where

import qualified Data.Set as Set
import DynamicSearch (bfs)
import System.Environment (getArgs)
import CardDB
import GameState
import MagicEffects
import Deck (Deck, readDeck)

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
