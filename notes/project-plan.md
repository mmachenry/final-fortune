Create a data model for a simplified game of magic that can represent the
state changes needed for cards that are relevant to the project. This data
model for a game state and cards will grow as new cards are added that require
more complicated relationships. Consider using the existing Magic model
developed by MedeaMelana. (https://github.com/MedeaMelana/Magic)

Write up a set of cards that are relevant to the project. Allow for cards to be
modeled in a more restrictive way than they might be true magic. For
efficiency's sake, we will allow for cards like Tinker to be implemented in a
way such that it can only search for Memory Jar, Time Vault, Voltaic Key, or
Black Lotus, but not any of the other artifacts that might be put into the
deck, like Mox Sapphire or Mox Ruby. This will allow the create of the deck to
hone the cards to not look for an insane number of possibilities and hopefully
complete more searches. In addition, cards will be allowed to cheat if need be.
Ad Nauseum, for example, is allowed to check the next card and only draw it if
it would not kill the player. Since this is a backtracking algorithm anyway,
that kind of lookahead cheating is really just a dynamic state space pruning.

We will create a best-first search algorithm that searches a random draw of a
game for a first-turn win. This algorithm will have to be guided by a fairly
complex set of rules for ordering the game states from best to worst. These
rules will likely have to be ad hoc added to a set of rules and be allowed to
be specific, rather than holistic. For exmaple, having more cards in hand is
better than having fewer, or playing a Gitaxian Probe before doing anything
else you might do is best. This might require a complicated algorithm for
coalating the rules but it will be easier to specify the rules than creating
one, monolithic compare function on two game states.

Deterministic randomness will be use to enumerate random trials so that they
can be easily retried, debugged, or stored by random seed, which is an integer,
and deck ID.

Make a state-space viewer/explorer for a give run of the algorithm, where a
human can watch the program execute through game states visually and see where
it went wrong (if at all), validate a victory if it happened, or potentially
suggest alterations to the heuristic that selects which path to follow first.
It should be easy to see what the next options are, which the game thinks is
the best to explore next, and how it's comparing them.

Create a database of decks and test runs of those decks where we can store the
results (win or loss) as well as and debug information about the test run. The
viewer shouled be able to pull results form the database and rerun them to do
debugging.

Create a distributed algorithm for running the search for a victory for many
decks and many random seeds on said decks. Parallelize the code running on one
machine so that not only can the search be distributed across many machines but
one machine can take several random seeds at once and try them in parallel.
A worker should take a dec and a range of random seeds. Try as many as it can
in parallel and then continue until all of the range it signed up to do is
completed and then report back. This could be a distributed client much like
Seti at home or Folding at home where people sign up to contribute off-cycle
CPU usage to the project.

Create deck construction rules that will obey the various tournament formats
and make a genetic algorithm that will mutate decks and optimize for the
highest probability of a first-turn win.

Allow users to submit decks that they think might be the highest probability of
a first-turn win. Provided the cards are implemented, allowing them of course
to use the more restricted versions of some cards or the cheater versions (see
above). Report back to the user what the probability of their deck winning on
the first turn is, allow them to explore the games that won or lost, run a
competition for the best user-submitted decks. A deck can easily be checked to
ensure that it's not a duplicate with something the algorithm has generated or
something already submitted. If it's not been submitted it counts and submitted
by the first user.

Use the best decks, whether user submitted or mutated, to guide the future
generations of the genetic algorithm.

Create a continuously running leader board for the best deck, best
user-submitted deck, etc. 

This will allow a user to, for example, see the currently winning deck, make a
one-card change that maybe the genetic algorithm had not yet considered, and
get credit for being the first to submit the new version of the deck. If it
actually is an improvement it will be the new leader and the genetic algorithm
will likely try mutations of it to improve on what they submitted.
