* Crowd source the decks. Make it a competition. See who can create a deck
  that's better than what the genetic algorithm has created. Allow players to
  implement cards they need if they are programmers or request that a card be
  implmented. A deck is uniquely your submission so long as no other deck in
  the database has an equal Multiset Card representation. Use the submitted
  decks as seeds to the genetic algorithm to see if it can iterate on submitted
  decks.

* Create a game viewer, potentially with the dot languag, which shows all of
  the pathways that a deck could follow given a particular random seed. Maybe
  make or find a graph viewer that allows you to selectively expand the view
  as you browse through the game.

* There needs to be individual card control over where the card goes after being
  cast for cards like Yawgmoth's Will, which gets removed from the game. It'd
  be nice if it were just overriden so most just go to the graveyard.

* Implement mana paying and adding with color

* Need a way to represent branching states for cards like Brain Storm where
  you have a choice about what to do that can't be codified before the game
  is played.

* Need a way to represent constant effects like Yawgmoth's Will, which changes
  both where one needs to look for options of what to play and also what happens
  to cards that go to the graveyard. 

* Add a list of effects that can be played to the state. This should contain
  the name of the effect, like Yawgmoth's Will.
  Check for said effects when playing the next states.

* Implement an end of turn effect that, for Yagmoth's Will, can clear its own
  effect out of the list of playable effects.

* Implement a new turn object, and have a list of next turns. At any point the
  player can pass the turn, call the end of turn effect, and play the next turn
  by copying the state. What is in the new turn object should probably be
  function that causes a loss for Final Fortune. Not sure how to propegate
  this with the new turn object.

* Make mana cost of cards a Maybe. They cannot be played in the traditional way
  like suspend cards and "Pact of ..." cards. But look through hand and
  graveyard for cards that have abilities when they are in those zones. Like
  Elvish Spirit Guide or suspend cards.

* How do we express the idea clearly that it's generally a good idea to have
  a spell on the stack when you pop Lion's Eye Diamond? We need to have a
  stack, however, we don't want to needlessly search the possibilities of
  putting an Ancestral Recall on the stack and then casting Dark Ritual vs.
  just simply playing the Dark Ritual, allowing it to resolve, and then playing
  the Ancestral Recall afterward.
