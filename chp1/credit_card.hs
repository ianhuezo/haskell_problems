--- First Haskell Project
--- Found from https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf
revDigits :: Integer -> [Integer]
revDigits n
    |n <= 0 = []
    |n > 0 = n `mod` 10 : revDigits( n `quot` 10)

toDigits :: Integer -> [Integer]
toDigits n
    |n <= 0 = []
    |n > 0 = reverse(n `mod` 10 : revDigits( n `quot` 10))

doubleEveryOther :: [Integer] -> [Integer] 

doubleEveryOther [] = []
doubleEveryOther (x:xs)
    | length xs == 0 = (x:xs)
    | length xs `mod` 2 == 1 = 2*x : doubleEveryOther xs
    | length xs `mod` 2 == 0 = x : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x >= 10 = sumDigits(toDigits x) + sumDigits xs
    | otherwise = x + sumDigits xs

validate :: Integer -> Bool
validate n
    | sumDigits( doubleEveryOther( toDigits(n))) `mod` 10 == 0 = True
    | otherwise = False