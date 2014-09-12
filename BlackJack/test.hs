import BlackJack.hs

-- Combines 2 different hands. (hand1 <+ hand2 = hand3)
(<+) :: Hand -> Hand -> Hand
Empty <+ hand2 = hand2
Add card hand1 <+ hand2 = Add card (hand1 <+ hand2)
