module CardDB (findCard) where

import qualified Data.MultiSet as MultiSet
import Data.List (find)
import Data.Maybe (fromMaybe)
import Mana
import GameState
import Card
import MagicEffects

findCard :: String -> Card
findCard name = fromMaybe
  (error ("Invalid card name: " ++ name))
  (find (\c->cardName c == name) cards)

cards :: [Card]
cards = [
   spell Sorcery "Tendrils of Agony" "2BB" $ stormWin 10
 , spell Instant "Brain Freeze" "1U" $ stormWin 18
 , spell Instant "Meditate" "2U" $ draw 4
 , spell Instant "Ancestral Recall" "U" $ draw 3
 , artifactMana "Black Lotus" "0" tapSacrifice $ addToManaPool (read "AAA")
 , artifactMana "Lion's Eye Diamond" "0" 
     (\this->sacrifice this >> discardHand)
     (addToManaPool (read "AAA")) 
 , spell Sorcery "Wheel of Fortune" "2R" $ discardHand >> draw 7
 -- Currently no facility to record opponent's handsize. It is assumed to stay
 -- at seven. If cards are added to the pool that can change it, we'll need
 -- to add a record of the current opponent hand size and reference it here.
 , spell Sorcery "Windfall" "2U" $ discardHand >> draw 7
 , spell Sorcery "Timetwister" "2U" $ do
     shuffleGraveyardHandIntoLibrary
     draw 7
 , spell Sorcery "Demonic Tutor" "1B" $ do
     card <- search library (const True)
     putIntoHand card
 , spell Sorcery "Regrowth" "1G" $ do
     card <- search graveyard (const True)
     putIntoHand card
 -- Make sure to reduce the set of possibilities by
 --   1) Only sacrificing unique permanents and only getting unique cards.
 --   2) Allowing for a parameterized set of targets to search for.
 , spell Sorcery "Tinker" "2U" $ do
     old <- search (MultiSet.toList . battlefield)
                   (isType Artifact . permanentCard)
     sacrifice old
     new <- search library (isType Artifact)
     putIntoPlay new
 , spell Instant "Dark Ritual" "B" $ addToManaPool (read "BBB")
 , spell Instant "Cabal Ritual" "1B" $ do
     addToManaPool (read "BBB")
     threshold (addToManaPool (read "B"))
 , spell Sorcery "Merchant Scroll" "1U" $ do
     card <- search library (\c->isType Instant c && isColor Blue c)
     putIntoHand card
 ]
