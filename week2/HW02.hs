{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches (x:xs) (y:ys)
    | x == y = 1  + r
    | otherwise = r
    where
        r = exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors ps = map  (\c -> length $ filter (c ==) ps) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = foldl (+) 0 (zipWith min (countColors xs) (countColors ys))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s c = 
    let exact = exactMatches s c
        nonExact = matches s c - exact
    in Move c exact nonExact
        

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess e n) posSecret = getMove posSecret guess == (Move guess e n)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m cs = filter (isConsistent m) cs

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = [[c] | c <- colors]
allCodes n = [x++y | x <- (allCodes 1), y <- (allCodes (n-1))]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
