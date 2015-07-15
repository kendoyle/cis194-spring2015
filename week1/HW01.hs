{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = mod x 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = quot x 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits x = [lastDigit x] ++ toRevDigits (dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x1:[]) = [x1]
doubleEveryOther (x1:x2:xs) = [x1] ++ [2*x2] ++ doubleEveryOther xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = dropLastDigit x + lastDigit x + sumDigits xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = 0 == mod (sumDigits (doubleEveryOther (toRevDigits n))) 10

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 start goal _    = [(start, goal)]
hanoi n start goal temp = hanoi (n-1) start temp goal ++ 
                          hanoi 1 start goal temp ++ 
						  hanoi (n-1) temp goal start

hanoiQ :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiQ 1 s g _  _  = [(s, g)]
hanoiQ 2 s g t1 _  = [(s, t1), (s, g), (t1, g)]
hanoiQ n s g t1 t2 = hanoiQ (n-2) s t2 g t1 ++
                     hanoiQ 2 s g t1 t2 ++
					 hanoiQ (n-2) t2 g t1 s
