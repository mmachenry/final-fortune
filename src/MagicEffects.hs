module MagicEffects where

import qualified Data.MultiSet as MultiSet
import Control.Monad (mzero, msum, mplus, guard, when)
import Control.Monad.State (modify, get, state)
import System.Random.Shuffle (shuffleM)
import System.Random (mkStdGen)
import GameState
import Mana
import Card

initGameState :: Int -> [Card] -> GameState
initGameState seed deck =
  let gs = GameState (initGame deck) (mkStdGen seed) []
  in head (runEffect gs (shuffleLibrary >> draw 7))

nextStates :: GameState -> [GameState]
nextStates gs@(GameState game _ _) = runEffect gs $
  msum (map playCard (MultiSet.distinctElems (hand game)))
  `mplus`
  msum (map activatePermanent (MultiSet.distinctElems (battlefield game)))

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
  guard $ stormCount >= n
  winGame

winGame :: Effect
winGame = modify (\g->g { isWin = True })

draw :: Int -> Effect
draw n = do
  lib <- fmap library get
  guard $ length lib >= n
  modify $ \g->g {
    library = drop n (library g),
    hand = MultiSet.union
             (MultiSet.fromList (take n (library g)))
             (hand g) }

discardHand :: Effect
discardHand = modify $ \g->g {
  hand = MultiSet.empty,
  graveyard = MultiSet.toList (hand g) ++ graveyard g }

shuffleGraveyardHandIntoLibrary :: Effect
shuffleGraveyardHandIntoLibrary = do
  modify $ \g->g {
    hand = MultiSet.empty,
    graveyard = [],
    library = MultiSet.toList (hand g) ++ graveyard g ++ library g }
  draw 7

-- TODO: This immediately resolves the stack rather than allowing that to be
-- a separate step. This will have to be changed if we want to allow for the
-- stacking of instant abilities which will be necessary for Lion's Eye
-- Diamond.
playCard :: Card -> Effect
playCard card = do
  if isType Land card
  then do hasPlayedLand <- fmap playedLand get
          guard $ not hasPlayedLand
          modify $ \g->g {
            hand = MultiSet.delete card (hand g),
            playedLand = True
            }
          putIntoPlay card
  else do payCost (cardCost card)
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
  then putIntoPlay topOfStack
  else do cardEffect topOfStack
          putInGraveyard topOfStack
  where popTheStack :: EffectM Card
        popTheStack = state $ \g->
          let (top:restOfStack) = stack g
          in (top, g { stack = restOfStack })

putInGraveyard :: Card -> Effect
putInGraveyard card = modify (\g->g { graveyard = card : graveyard g })

-- TODO This does not include the ability for a card to determine that it comes
-- into play tapped. It also should call any list of enters battlefield 
-- effects that currently are not suported.
putIntoPlay :: Card -> Effect
putIntoPlay card = do
  let perm = Permanent card False []
  modify (\g->g { battlefield = MultiSet.insert perm (battlefield g) })

putIntoHand :: Card -> Effect
putIntoHand card = modify $ \g -> g { hand = MultiSet.insert card (hand g) }

payCost :: Mana -> Effect
payCost manaCost = do
  game <- get
  let currentMana = manaPool game
  case payMana manaCost currentMana of
    Just newPool -> modify (\g->g { manaPool = newPool })
    Nothing -> mzero

addToManaPool :: String -> Effect
addToManaPool manaStr = modify $ \g->g {
  manaPool = addMana (readMana manaStr) (manaPool g) }

tap :: Permanent -> Effect
tap permanent = do
  guard $ not (permanentTapped permanent)
  let newPerm = permanent { permanentTapped = True }
  modify $ \g->g {
    battlefield =
      MultiSet.insert newPerm
        (MultiSet.delete permanent (battlefield g)) }

sacrifice :: Permanent -> Effect
sacrifice permanent = modify $ \g->g {
  battlefield = MultiSet.delete permanent (battlefield g),
  graveyard = permanentCard permanent : graveyard g }

tapSacrifice :: Permanent -> Effect
tapSacrifice permanent = do
  guard $ not (permanentTapped permanent)
  sacrifice permanent

shuffleLibrary :: Effect
shuffleLibrary = do
  oldLibrary <- fmap library get
  newLibrary <- shuffleM oldLibrary
  modify (\g->g { library = newLibrary })

-- TODO does not actually remove. use state for this.
search :: (Game -> [a]) -> (a -> Bool) -> EffectM a
search zone matching = do
  choices <- fmap zone get
  msum (map return (filter matching choices))

threshold :: Effect -> Effect
threshold effect = do
  gy <- fmap graveyard get
  when (length gy >= 7) effect

metalCraft :: Effect -> Effect
metalCraft effect = do
  permanents <- fmap battlefield get
  let num =
        MultiSet.size
          (MultiSet.filter (\p->isType Artifact (permanentCard p))
                           permanents)
  guard $ num >= 3
  effect
