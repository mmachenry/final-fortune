module CardDB (findCard) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Mana
import GameState
import MagicEffects

findCard :: String -> Card
findCard name = fromMaybe
  (error ("Invalid card name: " ++ name))
  (find (\c->cardName c == name) cards)

cards = [
   spell Sorcery "Tendrils of Agony" "2BB" $ stormWin 10
 , spell Instant "Brain Freeze" "1U" $ stormWin 18
 , spell Instant "Meditate" "2U" $ draw 4
 , spell Instant "Ancestral Recall" "U" $ draw 3
 , artifactMana "Black Lotus" "0" [(\this->
     tap this
     >>= sacrifice
     >>= onGs (addToManaPool (read "AAA")) )]
 ]

(|>) = flip ($)

-- Create a simple spell that works like an instant or a sorcery
-- with a name, mana cost, and effect from playing.
spell :: CardType -> String -> String -> Effect -> Card
spell t name costStr effect =
  let cost = read costStr
  in Card name (Mana.colors cost) [] [t] cost effect []

artifactMana name costStr effect =
  Card name [] [] [Artifact] (read costStr) doNothing effect

--artifactMana :: String -> String -> [Cost] -> String
--artifactMana name manaStr actCost effect =
--  Card name [] [Artifact] cost ...
