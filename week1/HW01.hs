{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits = map lastDigit . takeWhile (>0) . iterate dropLastDigit

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id, (*2)])

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toRevDigits

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (==0) . lastDigit . sumDigits . doubleEveryOther . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 0 = []
  | otherwise = hanoi (n-1) a c b
                ++ [(a,b)]
                ++ hanoi (n-1) c b a

-- Exercise 7 -----------------------------------------

-- Towers of Hanoi for four pegs

-- 1: move n-k to d (the extra)
-- 2: move the remaining k to b, using normal hanoi
-- 3: move n-k back from d to b
-- k is chosen such that n is the kth triangle number

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
 | n == 0 = []
 | otherwise  = hanoi4 (n-k) a d b c
                ++ hanoi k a b c
                ++ hanoi4 (n-k) d b a c
  where k = head [x | x <- [1..], x * (x + 1) `div` 2 >= n]
