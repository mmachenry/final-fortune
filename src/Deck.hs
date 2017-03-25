-- ^ This module contains the Deck data type, which is a list of card names.
-- It contains reader functions for parsing decks from files and later writing
-- them to file and HTML.
module Deck (
  Deck,
  readDeck,
  readCardCounts,
  expandDeck
  ) where

import Data.String.Utils

type Deck = [CardName]
type CardName = String
type CardCount = (Int, CardName)

-- ^ Read a deck from a string. The simple deck file format is pairs of numbers
-- followed by a free form string. The string is stripped of whitespace.
-- Comments are allowed in order to allow deck authors to categories their cards
-- or explain the inclusion or omission of anything in particular.
readDeck :: String -> Deck
readDeck = expandDeck . readCardCounts

-- ^ Read the deck file but produce a list of the pairs of card counts and
-- names instead of an expanded list of card names with duplicates. This is
-- useful for turning a deck file into HTML or manipulating and analyzing it
-- in a way that does not require drawing one by one from it.
readCardCounts :: String -> [CardCount]
readCardCounts = fmap readCardCount . lines

-- ^ Turns a list of card counts into a deck.
expandDeck :: [CardCount] -> Deck
expandDeck = concatMap (uncurry replicate)

-- Read one line of card count and card name and produce a CardCount.
-- TODO Should be rewritten to use Parsec.Language so that it's easy to
-- facilitate comments at the end of lines and detect syntax errors and be
-- generally a bit more robust.
readCardCount :: String -> CardCount
readCardCount str =
  case readsPrec 0 str of
    [(n,name)] -> (n,strip name)
    _ -> (0,"")
