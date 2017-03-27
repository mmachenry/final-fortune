module DynamicSearch where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (mapMaybe)
import GameState
import Deck
import CardDB
import MagicEffects -- might move nextStates into another module to avoid this import
import Debug

-- The number of wins that can be found for a given deck and a given number of
-- trials. The trails are randomized with a deterministic seed so that the the
-- results can be duplicated, studied, verified, or debugged.
numberOfWins :: Deck -> Int -> Int
numberOfWins deck trials = length $ mapMaybe (winningState deck) [1..trials]

-- True of the given deck wins if randomness is determined by the given seed.
-- The randomness is made deterministic so that results can be repeated.
winningState :: Deck -> Int -> Maybe GameState
winningState deck seed =
    bestFirstSearch (-1) Set.empty
  $ Set.singleton
  $ initGameState seed
  $ map findCard deck

-- TODO:
-- Analyze this algorithm to ensure we cannot duplicate states or if we can
-- we might need to memoize. Research best algorithms for handling this problem
-- and potentially use a library that already exists.
bestFirstSearch :: Int -> Set GameState -> Set GameState -> Maybe GameState
bestFirstSearch 0 _ _ = Nothing
bestFirstSearch depth seen states =
  if Set.null states
  then Nothing
  else let best = Set.findMax states
           rest = Set.deleteMax states
       in if Set.member best seen
          then bestFirstSearch (depth-1) seen rest
          else if isWin (gameStateGame best)
               then Just best
               else let ss = Set.fromList (nextStates best)
                    in bestFirstSearch (depth-1)
                                       (Set.insert best seen)
                                       (Set.union ss rest)

bfs :: Set.Set GameState -> IO (Maybe GameState)
bfs states = do
  print $ map (length . hand . gameStateGame) (Set.toList states)
  if Set.null states
  then return Nothing
  else let best = Set.findMax states
           rest = Set.deleteMax states
       in do putStrLn $ ppGs (gameStateGame best)
             if isWin (gameStateGame best)
             then return (Just best)
             else let ss = Set.fromList (nextStates best)
                  in do print $ length ss
                        str <- getLine
                        case str of
                          "show" -> mapM_ (putStrLn . ppGs . gameStateGame)
                                          (best : Set.toList rest)
                          "count" -> print $ Set.size rest
                          "best" -> print best
                          _ -> return ()
                        bfs (Set.union ss rest)
