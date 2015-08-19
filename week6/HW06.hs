{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

module HW06 where

import Data.List

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib (iterate (+1) 0)

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) (fibs2) (tail fibs2) 
-- tail is okay here because of lazy evaluation
--     1 : 1       : t0 -> 2 : t1 -> 3
--zw + 1 : t0 -> 2 : t1 -> 3 : t2 -> 5
--   = 2 : 3       : 5       : 8
-- laziness leading to self-memoization?

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)
-- where does Cons come from?
-- recursive definition?

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons e s) = (e:streamToList s)

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons e s) = Cons (f e) (fmap f s)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat n = Cons n (sRepeat n)

sRepeat' :: a -> Stream a
sRepeat' n = let s = Cons n s in s

sIterate :: (a -> a) -> a -> Stream a
sIterate rule seed = Cons seed (sIterate rule (rule seed))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons e s) ls = Cons e (sInterleave ls s) 

sTake :: Int -> Stream a -> [a]
sTake n = take n . streamToList

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = undefined

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate ((`mod` 2147483648) . (12345 +) . (1103515245 *)) 

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 235 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 67 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = Just (min1 xs x, max1 xs x)

min1 :: [Int] -> Int -> Int
min1 (x:xs) i  
    | x < i = min1 xs x
    | otherwise = min1 xs i
min1 [] i = i

max1 :: [Int] -> Int -> Int
max1 (x:xs) i  
    | x > i = max1 xs x
    | otherwise = max1 xs i
max1 [] i = i

--max2 :: [Int] -> Int

--minMax2 :: [Int] -> Maybe (Int, Int)




main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
