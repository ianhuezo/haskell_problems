---Looked this solution up as the best looking one in haskell(my solution looked imperative and this looked WAY better)
---
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

-- 1.  move n−1 discs from a to c using b as temporary storage
-- 2.  move the top disc from a to b
-- 3.  move n−1 discs from c to b using a as temporary storage

hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

