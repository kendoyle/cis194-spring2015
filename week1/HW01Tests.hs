-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (x, xs) = toRevDigits x == xs

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
			 [(123, [3,2,1]), (0, []), (10, [0, 1])]
		   ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (xs, ys) = ys == doubleEveryOther xs

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther 
             [([1, 2], [1, 4])]--, ([3, 6, 9], [3, 12, 9]), ([0, 0], [0, 0])] 
		   ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (xs, sum) = sum == sumDigits xs

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
             [([10,5,18,4], 19), ([], 0)] 
		   ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, result) = result == luhn n

ex5Tests :: [Test]
ex5Tests = [ Test "luhn Test" testLuhn
            [(5594589764218858, True), (1234567812345678, False)] 
           ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, a, b, c, ms) = ms == hanoi n a b c

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
             [(1, "a", "b", "c", [("a", "b")]),
			  (2, "x", "y", "z", [("x", "z"), ("x", "y"), ("z", "y")])]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
