module Debug where

import GameState
import qualified Data.MultiSet as MultiSet
import Data.List (intercalate)

ppGs :: Game -> String
ppGs g =
  "Game:" ++
  "\n storm: " ++ show (storm g) ++
  "\n library(" ++ show (length (library g)) ++ "): " ++
                   intercalate ";" (map cardName (take 3 (library g))) ++
  "\n  hand" ++ cl (MultiSet.toList (hand g)) ++
  "\n  battlefield(" ++ show (length (battlefield g)) ++ "): " ++
                       intercalate ";" (map ppPerm (MultiSet.toList (battlefield g))) ++
  "\n  graveyard" ++ cl (graveyard g) ++
  "\n"
  where cl l = "(" ++ show (length l) ++ "): " ++ intercalate ";" (map cardName l)
        ppPerm p = (if permanentTapped p then "Tapped " else "Untapped ") ++
                   cardName (permanentCard p)
