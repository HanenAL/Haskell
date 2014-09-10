module BlackJack where

import Cards
import Wrapper

hand2 = Add (Card { rank = Numeric 2, suit = Hearts })
          (Add (Card { rank = Jack, suit = Spades }) Empty)

-- size hand2
--    = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
--      Sends in (Card (Numeric 2) Hearts) as the first Card and (Add (Card Jack Spades) Empty) as    --      the next hand. Adds 1 to the size. The "Empty" will activate the base case and will simply add --      0 to the size of the hand.
--    = Adds 1 for every hand and continues until the next hand is "Empty".
--    = 2


aCard1 :: Card
aCard1 = Card { rank = Ace, suit = Hearts }

aCard2 :: Card
aCard2 = Card { rank = King, suit = Diamonds }

aHand :: Hand
aHand = Add aCard1 (Add aCard2 Empty)

empty :: Hand
empty = Empty

-- Use valueRank to determine the value of the current hand.
value :: Hand -> Integer
value Empty           = 0
value (Add (Card rank suit) hand) = valueRank rank + value hand

-- Values each rank, returns the value in numbers
valueRank :: Rank -> Integer
valueRank King        = 10
valueRank Queen       = 10
valueRank Jack        = 10
valueRank (Numeric x) = x
valueRank Ace         = 11

-- If the hand is valued more than 21, gameOver is set to True.
gameOver :: Hand -> Bool
gameOver hand | (value hand) <= 21 = False
gameOver hand | (value hand) > 21 = True

-- winner :: Hand -> Hand -> Player



data Player = Guest | Bank
          deriving (Show, Eq)
