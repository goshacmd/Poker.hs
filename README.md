# Poker.hs

```haskell
s_hc = makeHand [(Spades, Ace), (Diamonds, Jack), (Diamonds, Seven), (Hearts, Three), (Spades, Queen)]
s_sf = makeHand [(Hearts, Three), (Hearts, Four), (Hearts, Five), (Hearts, Six), (Hearts, Seven), (Hearts, Eight)]

matches s_hc -- => [HighCard A]
matches s_sf -- => [HighCard A, Straight 7, Flush Hearts 7, StraightFlush Hearts 7]
```
