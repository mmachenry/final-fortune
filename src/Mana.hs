-- TODO: need a way to model black lotus or lion's eye diamoner so that mana
-- that is added to the pool is of any color but must be all the same as each
-- other. This is trivial to add by simply give 5 options but this explodes the
-- search space needlessly.

module Mana where

import Data.Char
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

data Color = White | Blue | Black | Red | Green
  deriving (Eq, Ord, Show)

data ManaType = Colored Color | Phyrexian Color | Colorless | AnyColor
  deriving (Eq, Ord, Show)

type Mana = MultiSet ManaType

-- TODO fix: make not and instance, but create a String -> Mana function
-- symbols that must be read:
-- WUBRG, the colors
-- numbers, representing the amount of mana of any type
-- C, specifically colorless mana
-- (u/p), phyrexian mana (can be paid with 2 life or the color)
-- (r/g), multicolored mana (can be paid with either color)
readMana :: String -> Mana
readMana str =
  let (colorlessStr, colorStr) = span isDigit str
  in MultiSet.fromOccurList $ filter (\(_,n)-> n > 0) $ [
       (Colored White, length (filter (=='W') colorStr)),
       (Colored Blue, length (filter (=='U') colorStr)),
       (Colored Black, length (filter (=='B') colorStr)),
       (Colored Red, length (filter (=='R') colorStr)),
       (Colored Green, length (filter (=='G') colorStr)),
       (Colorless, if colorlessStr == ""
                   then 0
                   else read colorlessStr),
       (AnyColor, length (filter (=='A') colorStr))]

addMana :: Mana -> Mana -> Mana
addMana = MultiSet.union

-- This is complicated and will need to return many possibilities. I need
-- to allow for demotion of colored mana to colorless mana.
payMana :: Mana -> Mana -> Maybe Mana
payMana cost pool =
  let available = MultiSet.intersection cost pool
      amountLeft = MultiSet.difference pool available
      stillNeeded = MultiSet.difference cost available
  in if convertedManaCost stillNeeded <= MultiSet.occur AnyColor amountLeft
     then Just (MultiSet.difference amountLeft
                 (MultiSet.fromOccurList
                   [(AnyColor, convertedManaCost stillNeeded)]))
     else Nothing

convertedManaCost :: Mana -> Int
convertedManaCost = MultiSet.size

colors :: Mana -> [Color]
colors = MultiSet.distinctElems . MultiSet.mapMaybe manaTypeColor
  where manaTypeColor manaType = case manaType of
          Colored c -> Just c
          Phyrexian c -> Just c
          _ -> Nothing

emptyManaPool :: Mana
emptyManaPool = MultiSet.empty
