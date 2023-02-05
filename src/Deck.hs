module Deck where

data Face = Ace | Two | Three | Four | Five | Six |
            Seven | Eight | Nine | Ten | Jack | Queen | King
            deriving (Show, Eq, Enum)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Show, Eq, Enum)

data Card = Card Face Suit
            deriving (Show, Eq)

type Deck = [Card]

fullDeck :: [Card]
fullDeck = [Card face suit | suit <- [Clubs .. Spades], face <- [Ace .. King]]
