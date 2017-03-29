module CardDB (findCard) where

import qualified Data.MultiSet as MultiSet
import Control.Monad.State (get, modify)
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

 ---------------------
 -- Storm win
 ---------------------
   spell Sorcery "Tendrils of Agony" "2BB" $ stormWin 10
 , spell Instant "Brain Freeze" "1U" $ stormWin 18
 , spell Sorcery "Grapeshot" "1R" $ modify $ \g->
     let o = opponent g
         l = opponentLife o
         newLife = max (l - (storm g)) 0
         newO = o { opponentLife = newLife }
     in g { opponent = newO, isWin = newLife == 0 }

 ---------------------
 -- ARTIFACT FAST MANA
 ---------------------
 , artifactMana "Black Lotus" "0" tapSacrifice $ addToManaPool "AAA"
 , artifactMana "Lotus Petal" "0" tapSacrifice $ addToManaPool "A"
 , artifactMana "Lion's Eye Diamond" "0" 
     (\this->sacrifice this >> discardHand)
     (addToManaPool "AAA") 
 , artifactMana "Mox Pearl" "0" tap $ addToManaPool "W"
 , artifactMana "Mox Sapphire" "0" tap $ addToManaPool "U"
 , artifactMana "Mox Jet" "0" tap $ addToManaPool "B"
 , artifactMana "Mox Ruby" "0" tap $ addToManaPool "R"
 , artifactMana "Mox Emerald" "0" tap $ addToManaPool "G"
 , artifactMana "Mana Crypt" "0" tap $ addToManaPool "2"
 , artifactMana "Sol Ring" "1" tap $ addToManaPool "1"
 , artifactMana "Mana Vault" "1" tap $ addToManaPool "3"
 , artifactMana "Grim Monolith" "2" tap $ addToManaPool "3"
 -- make legendary and respect legend rule
 , artifactMana "Mox Opal" "0" tap $ metalCraft (addToManaPool "A")
 -- , artifactMana "Chrome Mox" 0 tap ...
 -- , artifactMana "Mox Diamond" 0 tap ...

 -----------------
 -- Card Advantage
 -----------------
 , spell Instant "Meditate" "2U" $ draw 4
 , spell Instant "Ancestral Recall" "U" $ draw 3
 , spell Sorcery "Wheel of Fortune" "2R" $ discardHand >> draw 7
 -- Currently no facility to record opponent's handsize. It is assumed to stay
 -- at seven. If cards are added to the pool that can change it, we'll need
 -- to add a record of the current opponent hand size and reference it here.
 , spell Sorcery "Windfall" "2U" $ discardHand >> draw 7
 , spell Sorcery "Timetwister" "2U" $ do
     shuffleGraveyardHandIntoLibrary
     draw 7

 --, spell Instant "Manamorphose" "1{R/G}" $ addToManaPool "AA"
 -- Make sure to reduce the set of possibilities by
 --   1) Only sacrificing unique permanents and only getting unique cards.
 --   2) Allowing for a parameterized set of targets to search for.
 , spell Sorcery "Tinker" "2U" $ do
     old <- search (MultiSet.distinctElems . battlefield)
                   (isType Artifact . permanentCard)
     sacrifice old
     new <- search library (isType Artifact)
     putIntoPlay new
     shuffleLibrary

 ------------
 -- Fast mana
 ------------
 , spell Instant "Dark Ritual" "B" $ addToManaPool "BBB"
 , spell Instant "Cabal Ritual" "1B" $ do
     addToManaPool "BBB"
     threshold (addToManaPool "B")
 , spell Sorcery "Inner Fire" "3R" $ do
     cardsInHand <- fmap hand get
     addToManaPool (replicate (length cardsInHand) 'R')
 , spell Instant "Pyretic Ritual" "1R" $ addToManaPool "RRR"
 , spell Instant "Rite of Flame" "R" $ do
     cardsInGy <- fmap graveyard get
     let flamesInGy = filter (\c->cardName c == "Rite of Flame") cardsInGy
     addToManaPool "RR"
     addToManaPool (replicate (length flamesInGy) 'R')
 , spell Instant "Seething Song" "2R" $ addToManaPool "RRRRR"

 --------
 -- Tutor
 --------
 , spell Sorcery "Merchant Scroll" "1U" $ do
     card <- search library (\c->isType Instant c && isColor Blue c)
     putIntoHand card
     shuffleLibrary
 , spell Sorcery "Demonic Tutor" "1B" $ do
     card <- search library (const True)
     putIntoHand card
     shuffleLibrary
 , spell Sorcery "Rhystic Tutor" "2B" $ do
     card <- search library (const True)
     putIntoHand card
     shuffleLibrary
 , spell Sorcery "Regrowth" "1G" $ do
     card <- search graveyard (const True)
     putIntoHand card

 -------
 -- Land
 -------
 , Card "Gemstone Mine" [] [] [Land] (readMana "0") (return ()) [
     ActivatedAbility tap (addToManaPool "A") ]
 , Card "Tolarian Academy" [] [Legendary] [Land] (readMana "0") (return ()) [
     ActivatedAbility tap $ do
       artifacts <- fmap battlefield get
       let numArtifacts =
             MultiSet.size
               (MultiSet.filter (isType Artifact . permanentCard) artifacts)
       addToManaPool (replicate numArtifacts 'U')
   ]
 ]
