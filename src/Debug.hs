module Debug where

import GameState
import qualified Data.MultiSet as MultiSet
import Data.List (intersperse)

ppGs :: Game -> String
ppGs g =
  "Game:" ++
  "\n library(" ++ show (length (library g)) ++ "): " ++
                   join (map cardName (take 3 (library g))) ++
  "\n  hand" ++ cl (MultiSet.toList (hand g)) ++
  "\n  battlefield(" ++ show (length (battlefield g)) ++ "): " ++
                       join (map ppPerm (MultiSet.toList (battlefield g))) ++
  "\n  graveyard" ++ cl (graveyard g) ++
  "\n"
  where cl l = "(" ++ show (length l) ++ "): " ++ join (map cardName l)
        join = concat . intersperse ";"
        ppPerm p = (if permanentTapped p then "Tapped " else "Untapped ") ++
                   cardName (permanentCard p)
