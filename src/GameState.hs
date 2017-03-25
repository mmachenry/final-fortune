{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameState where

import Control.Monad.Random (RandT, runRandT, MonadRandom)
import Control.Monad.Writer (WriterT, runWriterT, MonadWriter)
import Control.Monad.State (StateT, runStateT, MonadState)
import Control.Monad (MonadPlus)
import Control.Applicative (Alternative)
import System.Random (StdGen)
import Data.Function (on)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Mana

data GameState = GameState {
  gameStateGame :: Game,
  gameStateGen :: StdGen,
  gameStateHistory :: History
  } deriving (Show)

instance Eq GameState where
  (==) = (==) `on` gameStateGame

instance Ord GameState where
  compare = compare `on` gameStateGame

newtype EffectM a = EffectM (WriterT History (RandT StdGen (StateT Game [])) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus,
            MonadRandom, MonadWriter History, MonadState Game)

type Effect = EffectM ()
type History = [String]

runEffect :: GameState -> Effect -> [GameState]
runEffect (GameState game stdGen history) (EffectM effect) =
  let result = runStateT (runRandT (runWriterT effect) stdGen) game
  in map (\((((), newHist), newGen), newGame)->
           GameState newGame newGen (history ++ newHist))
         result

data Game = Game {
  life :: Int,
  manaPool :: Mana,
  storm :: Int,
  hand :: MultiSet Card,
  stack :: [Card],
  graveyard :: [Card],
  exiled :: MultiSet Card,
  battlefield :: MultiSet Permanent,
  library :: [Card],
  playedLand :: Bool,
  isWin :: Bool
  } deriving (Show)

instance Eq Game where
  a == b = life a == life b
           && manaPool a == manaPool b
           && storm a == storm b
           && hand a == hand b
           && stack a == stack b
           && graveyard a == graveyard b
           && exiled a == exiled b
           && battlefield a == battlefield b
           && library a == library b
           && playedLand a == playedLand b

-- TODO this is a total punt on ordering
instance Ord Game where
  compare g1 g2 =
    case (isWin g1, isWin g2) of
      (True, False) -> GT
      (False, True) -> LT
      (_, _) -> compareOn storm g1 g2
                `breakTie` compareOn battlefield g1 g2
                `breakTie` compareOn hand g1 g2
                `breakTie` compareOn manaPool g1 g2
                `breakTie` compareOn graveyard g1 g2
                `breakTie` compareOn library g1 g2
                `breakTie` compareOn stack g1 g2
                `breakTie` compareOn exiled g1 g2
                `breakTie` compareOn playedLand g1 g2

compareOn :: Ord b => (a -> b) -> a -> a -> Ordering
compareOn f a b = compare (f a) (f b)

breakTie :: Ordering -> Ordering -> Ordering
breakTie lhs rhs = if lhs == EQ then rhs else lhs

initGame :: [Card] -> Game
initGame cards = Game {
  life = 20,
  manaPool = emptyManaPool,
  storm = 0,
  hand = MultiSet.empty,
  stack = [],
  graveyard = [],
  exiled = MultiSet.empty,
  battlefield = MultiSet.empty,
  library = cards,
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
  cardActivatedAbilities :: [ActivatedAbility]
  --cardInHandAbilities :: [...],
  --cardGraveyardAbilities :: [...]
  }

data ActivatedAbility = ActivatedAbility {
  activatedAbilityCost :: Permanent -> Effect,
  activatedAbilityEffect :: Effect
  }

instance Ord Card where
  compare = compareOn cardName

instance Eq Card where
  (==) = (==) `on` cardName

instance Show Card where
  show card = "findCard \"" ++ cardName card ++ "\""

data Permanent = Permanent {
  permanentCard :: Card,
  permanentTapped :: Bool,
  permanentImprinted :: [Card]
  } deriving (Eq, Show, Ord)
