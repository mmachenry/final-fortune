-- TODO: need a way to model black lotus or lion's eye diamoner so that mana
-- that is added to the pool is of any color but must be all the same as each
-- other. This is trivial to add by simply give 5 options but this explodes the
-- search space needlessly.

module Mana where

import Data.Char

data Color = White | Blue | Black | Red | Green
  deriving (Eq)

-- TODO default ordering is not good. it should give an idea of how good this
-- state is.
data Mana = Mana {
  white :: Int,
  blue :: Int,
  black :: Int,
  red :: Int,
  green :: Int,
  colorless :: Int,
  anyColor :: Int
  } deriving (Show, Eq, Ord)

{-
instance Show Mana where
    show mana =
           (if colorless mana == 0 then "" else show (colorless mana))
        ++ replicate (anyColor mana) 'A'
        ++ replicate (white mana) 'W'
        ++ replicate (blue mana) 'U'
        ++ replicate (black mana) 'B'
        ++ replicate (red mana) 'R'
        ++ replicate (green mana) 'G'
-}

instance Read Mana where
    readsPrec _ str =
        let (colorlessStr, colorStr) = span isDigit str
            mana = Mana (length (filter (=='W') colorStr))
                        (length (filter (=='U') colorStr))
                        (length (filter (=='B') colorStr))
                        (length (filter (=='R') colorStr))
                        (length (filter (=='G') colorStr))
                        (if colorlessStr == "" then 0 else read colorlessStr)
                        (length (filter (=='A') colorStr))
        in [(mana,"")]

addMana :: Mana -> Mana -> Mana
addMana m1 m2 =
    Mana (white m1 + white m2) (blue m1 + blue m2) (black m1 + black m2)
         (red m1 + red m2) (green m1 + green m2) (colorless m1 + colorless m2)
         (anyColor m1 + anyColor m2)

-- TODO this is not really implemented
payMana :: Mana -> Mana -> Maybe Mana
payMana cost pool =
  if anyColor pool - convertedManaCost cost >= 0
  then Just $ pool {
         anyColor = anyColor pool - convertedManaCost cost
         }
  else Nothing

convertedManaCost :: Mana -> Int
convertedManaCost mana =
      white mana + blue mana + black mana + red mana + green mana
    + colorless mana + anyColor mana

colors :: Mana -> [Color]
colors _mana = undefined

emptyManaPool :: Mana
emptyManaPool = Mana 0 0 0 0 0 0 0

