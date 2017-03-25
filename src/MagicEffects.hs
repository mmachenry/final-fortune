module MagicEffects where

import qualified Data.MultiSet as MultiSet
import Control.Monad (mzero, msum, mplus)
import Control.Monad.State (modify, get, state)
import System.Random.Shuffle (shuffleM)
import Data.List (intersect)
import System.Random (mkStdGen)
import GameState
import Mana

initGameState :: Int -> [Card] -> GameState
initGameState seed deck =
  let gs = GameState (initGame deck) (mkStdGen seed) []
  in head (runEffect gs (shuffleLibrary >> draw 7))

nextStates :: GameState -> [GameState]
nextStates gs@(GameState game _ _) = runEffect gs $
  msum (map playCard (MultiSet.toList (hand game)))
  `mplus` msum (map activatePermanent (MultiSet.toList (battlefield game)))

activatePermanent :: Permanent -> Effect
activatePermanent permanent =
  let abilities = cardActivatedAbilities (permanentCard permanent)
  in msum (map (activateAbility permanent) abilities)

activateAbility :: Permanent -> ActivatedAbility -> Effect
activateAbility permanent (ActivatedAbility cost effect) = do
  cost permanent
  effect

stormWin :: Int -> Effect
stormWin n = do
  stormCount <- fmap storm get
  if stormCount >= n
  then winGame
  else mzero

winGame :: Effect
winGame = modify (\gs->gs { isWin = True })

draw :: Int -> Effect
draw n = do
  lib <- fmap library get
  if length lib >= n
  then modify (\gs->gs {
         library = drop n (library gs),
         hand = MultiSet.union
                  (MultiSet.fromList (take n (library gs)))
                  (hand gs) })
  else mzero

-- TODO: This immediately resolves the stack rather than allowing that to be
-- a separate step. This will have to be changed if we want to allow for the
-- stacking of instant abilities which will be necessary for Lion's Eye
-- Diamond.
playCard :: Card -> Effect
playCard card = do
  payCost (cardCost card)
  modify $ \g->g {
    hand = MultiSet.delete card (hand g),
    stack = card : stack g,
    storm = storm g + 1 }
  resolveStack

-- TODO: This will fail to call the effect of a permanent. That's bad but
-- should they be called? Should they even have effects? Perhaps there should
-- just be different constructors for cards that are spells or permanents?
resolveStack :: Effect
resolveStack = do
  topOfStack <- popTheStack
  if isPermanentCard topOfStack
    then enterBattlefield topOfStack
    else do cardEffect topOfStack
            putInGraveyard topOfStack
  where popTheStack :: EffectM Card
        popTheStack = state $ \g->
          let (top:restOfStack) = stack g
          in (top, g { stack = restOfStack })
        putInGraveyard :: Card -> Effect
        putInGraveyard card =
          modify (\g->g { graveyard = card : graveyard g })

-- TODO This does not include the ability for a card to determine that it comes
-- into play tapped. It also should call any list of enters battlefield 
-- effects that currently are not suported.
enterBattlefield :: Card -> Effect
enterBattlefield card = do
  let perm = Permanent card False []
  modify (\g->g { battlefield = MultiSet.insert perm (battlefield g) })

isPermanentCard :: Card -> Bool
isPermanentCard card =
  length ( intersect (cardType card)
                     [Artifact, Enchantment, Creature, Planeswalker, Land] ) > 0

payCost :: Mana -> Effect
payCost manaCost = do
  game <- get
  let currentMana = manaPool game
  case payMana manaCost currentMana of
    Just newPool -> modify (\g->g { manaPool = newPool })
    Nothing -> mzero

doNothing :: Effect
doNothing = return ()

addToManaPool :: Mana -> Effect
addToManaPool mana = modify $ \g->g { manaPool = addMana mana (manaPool g) }

-- TODO write with guard
tap :: Permanent -> Effect
tap permanent =
  if permanentTapped permanent
  then mzero
  else do let newPerm = permanent { permanentTapped = True }
          modify $ \g->g {
            battlefield =
              MultiSet.insert newPerm
                (MultiSet.delete permanent (battlefield g)) }

sacrifice :: Permanent -> Effect
sacrifice permanent =
  modify $ \g->g {
    battlefield = MultiSet.delete permanent (battlefield g),
    graveyard = permanentCard permanent : graveyard g }

-- TODO write with guard
tapSacrifice :: Permanent -> Effect
tapSacrifice permanent = 
  if permanentTapped permanent
  then mzero
  else sacrifice permanent

shuffleLibrary :: Effect
shuffleLibrary = do
  oldLibrary <- fmap library get
  newLibrary <- shuffleM oldLibrary
  modify (\g->g { library = newLibrary })

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
