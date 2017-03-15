module GameState where

import Control.Monad.Random.Class (MonadRandom)
import System.Random.Shuffle (shuffleM)
import System.Random (StdGen, mkStdGen)
import Control.Monad.Random (runRand)
import Data.Function (on)
import Data.List (delete, intersect, intersperse)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Mana
import Deck

type Effect = GameState -> [GameState]
type PermanentEffect = PermanentState -> [PermanentState]

data GameState = GameState {
  life :: Int,
  manaPool :: Mana,
  storm :: Int,
  hand :: MultiSet Card,
  stack :: [Card],
  graveyard :: [Card],
  exiled :: MultiSet Card,
  battlefield :: MultiSet Permanent,
  library :: [Card],
  history :: [String],
  playedLand :: Bool,
  isWin :: Bool
  } deriving (Show)

data PermanentState = PermanentState {
  permanentStatePermanent :: Permanent,
  permanentStateGameState :: GameState
  } deriving (Show)

instance Eq GameState where
  a == b = life a == life b
           && manaPool a == manaPool b
           && storm a == storm b
           && hand a == hand b
           && stack a == stack b
           && graveyard a == graveyard b
           && exiled a == exiled b
           && battlefield a == battlefield b
           && library a == library b
           && history a == history b
           && playedLand a == playedLand b
           && isWin a == isWin b

-- TODO this is a total punt on ordering
instance Ord GameState where
  compare gs1 gs2 =
    case (isWin gs1, isWin gs1) of
      (True, False) -> GT
      (False, True) -> LT
      (_, _) -> compareOn storm gs1 gs2
                `breakTie` compareOn battlefield gs1 gs2
                `breakTie` compareOn hand gs1 gs2
                `breakTie` compareOn manaPool gs1 gs2
                `breakTie` compareOn graveyard gs1 gs2
                `breakTie` compareOn library gs1 gs2
                `breakTie` compareOn stack gs1 gs2
                `breakTie` compareOn exiled gs1 gs2
                `breakTie` compareOn history gs1 gs2
                `breakTie` compareOn playedLand gs1 gs2

compareOn f a b = compare (f a) (f b)

breakTie :: Ordering -> Ordering -> Ordering
breakTie rhs lhs = if rhs == EQ then lhs else rhs

initGameState :: Int -> [Card] -> GameState
initGameState seed cards =
  let (library, newGen) = runRand (shuffleM cards) (mkStdGen seed)
      startingHandSize = 7
  in GameState {
       -- stdGen = newGen,
       life = 20,
       manaPool = emptyManaPool,
       storm = 0,
       hand = MultiSet.fromList (take startingHandSize library),
       stack = [],
       graveyard = [],
       exiled = MultiSet.empty,
       battlefield = MultiSet.empty,
       library = drop startingHandSize library,
       history = [],
       playedLand = False,
       isWin = False
       }

data CardType =
    Artifact
  | Creature
  | Enchantment
  | Instant
  | Land
  | Planeswalker
  | Sorcery
  deriving (Eq, Show)

data CardSuperType = Basic | Legendary | Ongoing | Snow | World
  deriving (Eq, Show)

data Card = Card {
  cardName :: String,
  cardColors :: [Color],
  cardSuperType :: [CardSuperType],
  cardType :: [CardType],
  cardCost :: Mana,
  cardEffect :: Effect,
  cardActivatedAbilities :: [PermanentEffect]
  --cardInHandAbilities :: [...],
  --cardGraveyardAbilities :: [...]
  }

instance Ord Card where
  compare = compareOn cardName

instance Eq Card where
  (==) = (==) `on` cardName

data Permanent = Permanent {
  permanentCard :: Card,
  permanentTapped :: Bool,
  permanentImprinted :: [Card]
  } deriving (Eq, Show, Ord)

--instance Eq GameState where
--  gs1 == gs2 = undefined

--instance Ord GameState where
--  gs1 <= gs2 = undefined

nextStates :: GameState -> [GameState]
nextStates s =
     concatMap (flip playCard s) (hand s)
  ++ concatMap (flip activatePermanent s) (battlefield s)

-- TODO: This immediately resolves the stack rather than allowing that to be
-- a separate step. This will have to be changed if we want to allow for the
-- stacking of instant abilities which will be necessary for Lion's Eye
-- Diamond.
playCard :: Card -> Effect
playCard card gameState = do
  s1 <- payCost (cardCost card) gameState
  let s2 = s1 { hand = MultiSet.delete card (hand s1),
                stack = card : stack s1,
                storm = storm gameState + 1 }
  resolveStack s2

-- TODO: This will fail to call the effect of a permanent. That's bad but
-- should they be called? Should they even have effects? Perhaps there should
-- just be different constructors for cards that are spells or permanents?
resolveStack :: Effect
resolveStack gameState =
  let (topOfStack:restOfStack) = stack gameState
      newState = gameState { stack = restOfStack }
  in if isPermanentCard topOfStack
     then enterBattlefield topOfStack newState
     else do s <- cardEffect topOfStack newState
             return $ s { graveyard = topOfStack : graveyard s }

-- TODO This does not include the ability for a card to determine that it comes
-- into play tapped. It also should call any list of enters battlefield 
-- effects that currently are not suported.
enterBattlefield :: Card -> Effect
enterBattlefield card gs =
  let perm = Permanent card False []
  in [ gs { battlefield = MultiSet.insert perm (battlefield gs) } ]

isPermanentCard card =
  length ( intersect (cardType card)
                     [Artifact, Enchantment, Creature, Planeswalker, Land] ) > 0

payCost :: Mana -> Effect
payCost manaCost gameState =
  case payMana manaCost (manaPool gameState) of
    Just newPool -> [gameState { manaPool = newPool }]
    Nothing -> []

activatePermanent :: Permanent -> Effect
activatePermanent permanent gameState =
  let card = permanentCard permanent
  in do ability <- cardActivatedAbilities card
        map permanentStateGameState $
            ability (PermanentState permanent gameState)

doNothing :: Effect
doNothing gs = pure gs

addToManaPool :: Mana -> GameState -> GameState
addToManaPool mana gameState = gameState {
  manaPool = addMana mana (manaPool gameState)
  }

onGs :: (GameState -> GameState) -> PermanentEffect
onGs f (PermanentState p gs) = [PermanentState p (f gs)]

tap :: PermanentEffect
tap (PermanentState permanent gameState) =
  if permanentTapped permanent
  then []
  else let newPerm = permanent { permanentTapped = True }
       in [ PermanentState newPerm (gameState {
              battlefield =
                MultiSet.insert newPerm
                  (MultiSet.delete permanent (battlefield gameState)) } )
            ]
  where tapPerm p = p { permanentTapped = True }

sacrifice :: PermanentEffect
sacrifice (PermanentState permanent gameState) = [
  PermanentState permanent (
  gameState {
    battlefield = MultiSet.delete permanent (battlefield gameState),
    graveyard = permanentCard permanent : graveyard gameState
  }) ]

ppGs gs =
  "GameState:" ++
  "\n library(" ++ show (length (library gs)) ++ "): " ++
                   join (map cardName (take 3 (library gs))) ++
  "\n  hand" ++ cl (MultiSet.toList (hand gs)) ++
  "\n  battlefield(" ++ show (length (battlefield gs)) ++ "): " ++
                       join (map ppPerm (MultiSet.toList (battlefield gs))) ++
  "\n  graveyard" ++ cl (graveyard gs) ++
  "\n"
  where cl l = "(" ++ show (length l) ++ "): " ++ join (map cardName l)
        join = concat . intersperse ";"
        ppPerm p = (if permanentTapped p then "Tapped " else "Untapped ") ++
                   cardName (permanentCard p)

instance Show Card where
  show card = "findCard \"" ++ cardName card ++ "\""

