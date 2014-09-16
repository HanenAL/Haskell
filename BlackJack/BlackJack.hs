module BlackJack where

import Cards
import Wrapper
import Test.QuickCheck

hand2 = Add (Card { rank = Numeric 2, suit = Hearts })
          (Add (Card { rank = Jack, suit = Spades }) Empty)

-- size hand2
-- = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
-- = 1 + size (Add (Card Jack Spades) Empty)
-- = 2 + size Empty
-- = 2 + 0
-- = 2

aCard1 :: Card
aCard1 = Card { rank = Ace, suit = Hearts }

aCard2 :: Card
aCard2 = Card { rank = King, suit = Diamonds }

aCard3 :: Card
aCard3 = Card { rank = Queen, suit = Clubs }

aHand1 :: Hand
aHand1 = Add aCard2 (Add aCard1 (Add aCard2 (Add aCard3 Empty)))

aHand2 :: Hand
aHand2 = Add aCard3 (Add aCard2 (Add aCard1 Empty))

aHand3 :: Hand
aHand3 = Add aCard1 Empty

empty :: Hand
empty = Empty

-- Use valueRank to determine the value of the current hand.
totalValue :: Hand -> Integer
totalValue Empty                       = 0
totalValue (Add (Card rank _) hand) = valueRank rank + totalValue hand

-- fixes the value of the hand if it is more than 21 and it contains ace/s.
value :: Hand -> Integer
value Empty                    = 0
value hand 
  | totalValue hand <= 21 = totalValue hand
  | totalValue hand > 21  = totalValue hand - (10 * numberOfAces hand)

-- Values each rank, returns the value in numbers
valueRank :: Rank -> Integer
gameOver hand = value hand > 21
valueRank King        = 10
valueRank Queen       = 10
valueRank Jack        = 10
valueRank (Numeric x) = x
valueRank Ace         = 11

-- Values a Card given the rank.
valueCard :: Card -> Integer
valueCard (Card rank _) = valueRank rank 

-- Counts the number of Aces in the current hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add (Card _ _) hand)   = 0 + numberOfAces hand

-- If the hand is valued more than 21, gameOver is set to True.
gameOver :: Hand -> Bool

-- WHO WILL WIN!
winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest == True = Bank
  | gameOver bank  ==  True = Guest
  | value guest > value bank  = Guest 
  | value guest <= value bank = Bank


-- Combines 2 different hands. (hand1 <+ hand2 = hand3)
(<+) :: Hand -> Hand -> Hand
hand1 <+ Empty = hand1
Empty <+ hand2 = hand2
Add card hand1 <+ hand2 = Add card (hand1 <+ hand2)

-- Property, is (<+) associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- Property, is the size different
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size (p1 <+ p2) == size p1 + size p2

-- Combines all the complete suits into one hand creating a deck of
-- 52 cards.
fullDeck :: Hand
fullDeck = completeSuit ranks Hearts   <+
           completeSuit ranks Spades   <+
           completeSuit ranks Diamonds <+
           completeSuit ranks Clubs

-- List of all the existing ranks.
ranks :: [Rank]
ranks =  [Ace, King, Queen, Jack]++[Numeric x | x <- [2..10]]

-- Given a suit and a list of ranks (the list of all the ranks above)
-- makes a complete set of cards for the said suit.
completeSuit :: [Rank] -> Suit -> Hand
completeSuit [] suit = Empty
completeSuit (x:xs) suit = (Add (Card x suit) Empty) <+ completeSuit xs suit

-- draw :: Hand -> Hand -> (Hand, Hand)
