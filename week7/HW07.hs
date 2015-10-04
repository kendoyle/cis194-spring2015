{-# LANGUAGE MonadComprehensions, RecordWildCards, TypeSynonymInstances, FlexibleInstances  #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
    a <- ma
    return $ f a

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i0 i1 as = 
    let swapV' v0 v1 = as//[(i0, v1),(i1,v0)]
    in liftM2 swapV' (as !? i0) (as !? i1)
    
-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as = sequence $ map f as

getElts :: [Int] -> Vector a -> Maybe [a]
getElts is vec = mapM (vec !?) is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a) --IO (Maybe a)
randomElt vec = do
    i <- getRandomR (0, (V.length vec) - 1)
    return $ vec !? i

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n = V.replicateM n . getRandomR

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a) -- IO (Vector a)
shuffle vec = 
    let shuffle' 0 vec' = do return $ vec'
        shuffle' i vec' = do
            j <- getRandomR (0, i)
            v <- shuffle' (i - 1) (vec'//[(i, vec' ! j), (j, vec' ! i)])
            return v
    in shuffle' ((V.length vec) - 1) vec

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec i = ((V.filter (< pvt) vec), pvt, V.filter (pvt <=) vec)
    where pvt = vec ! i

partitionAt' :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt' vec i = 
    let pvt = vec ! i
        gte_pt = (\i' x -> i' /= i && x >= pvt)
    in ((V.filter (< pvt) vec), pvt, (V.ifilter gte_pt vec))

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vec = 
    if V.null vec 
    then vec
    else
        let x = V.head vec
            tl = V.tail vec
        in qsort [ y | y <- tl, y < x ]
            <> cons x (qsort [ y | y <- tl, y >= x ])

-- Exercise 8 -----------------------------------------

--qsortR :: Ord a => Vector a -> Vector a
qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vec = 
    if V.null vec 
    then do return vec
    else do
        i <- getRandomR (0, (V.length vec) - 1)
        let (los, pvt, his) = (partitionAt' vec i)
        his' <- qsortR his
        los' <- qsortR los
        return $ los' <> cons pvt his'

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select rank vec 
    | V.null vec = do return Nothing
    | rank >= V.length vec = do return Nothing 
    | otherwise  = do 
        i <- getRandomR (0, (V.length vec) - 1)
        let (los, pvt, his) = (partitionAt' vec i)
        let lo_len = V.length los
        case compare rank lo_len of
            EQ -> return $ Just pvt
            LT -> do 
                t <- select rank los
                return $ t
            GT -> do
                t <- select (rank - lo_len - 1) his
                return $ t

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card l s | s <- suits, l <- labels]

--type RDeck = Rnd Deck
--instance Show RDeck where
    --show rd = do
        --d <- rd
        --return (V.foldl' (++) "" (V.map (\ (Card l s) -> show s ++ show l ++ ", ") d))

--deckToString :: Deck -> String
--deckToString = V.foldl' (++) "" . V.map (\ (Card l s) -> show s ++ show l ++ ", ")

--instance Show (Rnd (Vector Card)) where
    --show rd = do
        --d <- rd
        --return $ deckToString d

-- or evalRandIO is a thing...

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck 
    | V.null deck = Nothing
    | otherwise = Just (V.head deck, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n deck
    | n == 0 = Just ([], deck)
    | V.null deck = Nothing
    | otherwise = do
        (n_card, deck') <- nextCard deck 
        (r_cards, deck'') <- getCards (n-1) deck'
        return $ ((n_card:r_cards), deck'')

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
