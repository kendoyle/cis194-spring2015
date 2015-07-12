{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Control.Applicative ((<*>))
import Data.List
import Data.Function

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
exactMatches n = length . filter id . zipWith (==) n

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times it occurs in ys
countColors :: Code -> [Int]
countColors c = map (exactMatches c . replicate 6) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c g = sum $ zipWith min (countColors c) (countColors g)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove c g = Move g x y
  where x = exactMatches c g
        y = matches c g - x

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move g _ _) c = getMove c g == m

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m = filter (isConsistent m)

-- Exercise 6 -----------------------------------------

extendCode :: [Code] -> [Code]
extendCode cs = map (:) colors <*> cs

allCodes :: Int -> [Code]
allCodes n = (!!n) $ iterate extendCode [[]]

-- Exercise 7 -----------------------------------------

applyFirstGuess :: Code -> ([Move], [Code]) -> ([Move], [Code])
applyFirstGuess c (ms, g:gs) = (m:ms, filterCodes m gs)
  where m = getMove c g

solve :: Code -> [Move]
solve c = reverse . fst . head $
          dropWhile ((>0) . length . snd) $
          iterate (applyFirstGuess c) ([], start)
        where start = allCodes (length c)

-- Bonus ----------------------------------------------

-- c is the secret code
-- u is the set of unused guesses
-- s is the set of still-viable guesses
-- g is the guess

-- what possible moves could result from a guess?
-- note: n-1 colors and 1 white is not possible
possibleMoves :: Code -> [Move]
possibleMoves g = [Move g x y | x <- [0..n], y <- [0..n], x + y <= n, (x,y) /= (n-1,1)]
  where n = length g

-- the score for a move is the number of possibilities it would eliminate
scoreForMove :: [Code] -> Move -> Int
scoreForMove s m = length s - length (filterCodes m s)

-- the score for a guess is the minimum number of possibilities its move would
-- eliminate
score :: [Code] -> Code -> Int
score s g = minimum $ map (scoreForMove s) (possibleMoves g)

-- the next guess is a guess that has the maximum score, preferentially taking
-- the guess from the remaining set s
nextGuess :: [Code] -> [Code] -> Code
nextGuess u s = g
  where scored = zip u (map (score s) u)
        highest = map fst $ last $
                  groupBy ((==) `on` snd) $ sortBy (compare `on` snd) scored
        s' = s `intersect` highest
        g = if null s' then head highest else head s'

-- knuth's algorithm for mastermind
knuth :: Code -> [Code] -> [Code] -> Code -> [Move]
knuth c u s g = let m@(Move _ color _) = getMove c g
                    s' = filterCodes m s
                    u' = delete g u
                    g' = nextGuess u' s'
                in if color == length c
                   then [m]
                   else m : knuth c u' s' g'

-- we start with the guess [Red, Red, Green, Green] (Knuth)
fiveGuess :: Code -> [Move]
fiveGuess c = knuth c a a g
  where a = allCodes (length c)
        g = [Red, Red, Green, Green]
