{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

-- equality of polynomials has to deal with trailing zero coefficients
instance (Num a, Eq a) => Eq (Poly a) where
    P [] == P [] = True
    P (a:as) == P (b:bs) = a == b && P as == P bs
    P [] == P (0:bs) = P [] == P bs
    P (0:bs) == P [] = P [] == P bs
    P _ == P _ = False

-- Exercise 3 -----------------------------------------

showCoeff :: (Num a, Eq a, Show a) => a -> String
showCoeff 1 = ""
showCoeff (-1) = "-"
showCoeff n = show n

showDegree :: Integer -> String
showDegree 0 = ""
showDegree 1 = "x"
showDegree n = "x^" ++ show n

showTerm :: (Num a, Eq a, Show a) => a -> Integer -> String
showTerm c d
  | c == 0 = ""
  | d == 0 = show c
  | otherwise = showCoeff c ++ showDegree d

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P cs) = if null s then "0" else s
      where s = intercalate " + " $ reverse $ filter (not . null) $ zipWith showTerm cs [0..]

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
P [] `plus` P [] = P []
P (a:as) `plus` P (b:bs) = P (a+b : rest) where (P rest) = P as + P bs
P (a:as) `plus` P [] = P (a:as)
P [] `plus` P (b:bs) = P (b:bs)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
P as `times` P bs = sum $ map P $ zipWith map (map (*) as) (iterate (0:) bs)

{-
-- Alternative method for multiplication
times :: Num a => Poly a -> Poly a -> Poly a
P (f:ft) `times` P gs@(g:gt) = P (f*g : rest)
  where (P rest) = (P gt `times` P [f]) `plus`
                   (P ft `times` P gs)
_ `times` _ = P []
-}

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P as) = P (map negate as)
    fromInteger a = P [fromInteger a]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P cs) v = sum $ zipWith (*) cs (iterate (*v) 1)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n = (!!n) . iterate deriv

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P (_:cs)) = P (zipWith (*) cs [1..])
    deriv (P []) = P []
