module Card where

import GameState
import Mana

-- Create a simple spell that works like an instant or a sorcery
-- with a name, mana cost, and effect from playing.
spell :: CardType -> String -> String -> Effect -> Card
spell t name costStr effect =
  let cost = read costStr
  in Card name (Mana.colors cost) [] [t] cost effect []

artifactMana :: String -> String -> (Permanent -> Effect) -> Effect -> Card
artifactMana name costStr cost effect =
  Card name [] [] [Artifact] (read costStr) (return ()) [
    ActivatedAbility cost effect]

isType :: CardType -> Card -> Bool
isType ct card = ct `elem` cardType card

isColor :: Color -> Card -> Bool
isColor color card = color `elem` cardColors card

isPermanentCard :: Card -> Bool
isPermanentCard card =
  any (`elem` cardType card)
      [Artifact, Enchantment, Creature, Planeswalker, Land]
