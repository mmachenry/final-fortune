module MagicEffects where

import GameState
import qualified Data.MultiSet as MultiSet

stormWin :: Int -> Effect
stormWin n gameState =
  if storm gameState >= n
  then [ gameState { isWin = True } ]
  else []

draw :: Int -> Effect
draw n gameState =
  if length (library gameState) >= n
  then [ gameState { library = drop n (library gameState),
                   hand = MultiSet.union
                            (MultiSet.fromList (take n (library gameState)))
                            (hand gameState) } ]
  else []

{-
----------------------------------------------------------------------
-- Common effects abstracted
----------------------------------------------------------------------
payMana :: Mana -> Effect
payMana manaCost state =
    if convertedManaCost manaCost > convertedManaCost (manaPool state)
    then Set.empty
    else Set.singleton state {
             manaPool = Mana.difference (manaPool state) manaCost
             }

addToManaPool mana state = Set.singleton state {
    manaPool = Mana.sum mana (manaPool state)
    }

payLife :: Int -> Effect
payLife amount state = Set.singleton state {
    life = (life state) - amount
    }

draw n state = Set.singleton state {
    hand = insertAll (take n (library state)) (hand state),
    library = drop n (library state)
    }

discardHand state = Set.singleton state {
    hand = MultiSet.empty,
    graveyard = MultiSet.union (hand state) (graveyard state)
    }

shuffleGraveyardLibrary state =
    let newLib = MultiSet.toList (graveyard state) ++ (library state)
        (shuffled, gen) = shuffle (stdGen state) newLib
    in Set.singleton state {
           stdGen = gen,
           library = shuffled,
           graveyard = MultiSet.empty
           }

exileTop n state = Set.singleton state {
    library = drop n (library state),
    exiled = insertAll (take n (library state)) (exiled state)
    }

stormWin n state =
    if storm state >= n
    then Set.singleton state { isWin = True }
    else Set.empty

threshold effect thresholdEffect state =
    if MultiSet.size (graveyard state) >= 7
    then thresholdEffect state
    else effect state

metalCraft effect state =
    if MultiSet.size (MultiSet.filter ((elem Artifact).cardType.permanentCard)
                                      (battlefield state)) >= 3
    then effect state
    else Set.empty

doNothing state = Set.empty
-}
