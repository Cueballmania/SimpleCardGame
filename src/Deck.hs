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

cardValue :: Card -> Int
cardValue (Card face _) =
    case face of
        Ace   -> 1
        Two   -> 2
        Three -> 3
        Four  -> 4
        Five  -> 5
        Six   -> 6
        Seven -> 7
        Eight -> 8
        Nine  -> 9
        _     -> 10


