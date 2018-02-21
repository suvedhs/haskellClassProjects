-- This is head comment (comment should be preceded by two dashes)
-- Assignment 2, CSCE 314
-- Student Name: Suvedh Srikanth
-- UIN: 325001057
-- (Acknowledge any help received here)

module Main where

import Test.HUnit
import Data.List
import System.Exit

-- Problem 2
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


-- Problem 3
myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct c = head c * myProduct (drop 1 c)



-- Problem 4
c = []
flatten :: [[a]] -> [a]
flatten [[]] = []
flatten a = c
    where
        c = concat a

z = 0
-- Problem 5
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = z
  where
    z = 1 + myLength (xs)

-- Problem 6
quicksort :: (Ord t) => [t] -> [t]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]


-- Problem 7
isElement :: Eq a => a -> [a] -> Bool
isElement e [] = False
isElement el (x:xs) = (el == x) || isElement el xs


-- Problem 8
isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)


-- Problem 9
riffle :: [a] -> [a] -> [a]
riffle [] [] = []
riffle (x:xs) (y:ys) = x : y : riffle xs ys


-- Problem 10
shuffle :: Int -> [a] -> [a]
shuffle b [] = []
shuffle 0 a = a
shuffle b a = shuffle (b-1) (riffle (take (myLength a `div` 2) a) (drop (myLength a `div` 2) a))


-- Problem 11
factors n = [z | z <- [1..n-1], n `mod` z == 0]
perfects :: Int -> [Int]
perfects n = [z | z <- [1..n], sum (factors z) == z]


--Problem 12
replicate :: Int -> a-> [a]
replicate 0 z = []
replicate n z = [z | _ <- [1..n]]



myTestList =
  TestList [
    "fibonacci" ~: fibonacci 4 ~=? 3

    , "myProduct" ~: myProduct [1..10] ~=? 3628800

    , "flatten 1" ~: flatten [[]::[Int]] ~=? []
    , "flatten 2" ~: flatten [[]::[Int], [], []] ~=? []
    , "flatten 3" ~: flatten [[1], [2, 3, 4], [], [5, 6]] ~=? [1, 2, 3, 4, 5, 6]
    , "myLength" ~: myLength [1, 2, 3] ~=? 3

    , "quicksort 1" ~: quicksort [3, 2, 5, 1, 6] ~=? [1,2,3,5,6]
    , "quicksort 2" ~: quicksort "howdy" ~=? "dhowy"

    , "isElement 1" ~: (isElement 'c' "abcd") ~=? True
    , "isElement 2" ~: (isElement 'e' "abcd") ~=? False

    , "isort 1" ~: isort [3, 2, 5, 1, 6] ~=? [1,2,3,5,6]

    , "riffle 1" ~: riffle [3, 2, 5] [0, 1, 6] ~=? [3,0,2,1,5,6]

    , "shuffle 1" ~: shuffle  2 [3, 2, 5, 0, 1, 6] ~=? [3,1,0,5,2,6]

    , "perfects 1" ~: perfects 7 ~=? [6]

    , "replicate 1" ~: Main.replicate 7 "howdy" ~=? ["howdy","howdy","howdy","howdy","howdy","howdy","howdy"]


    ]

main = do c <- runTestTT myTestList

          putStrLn $ show c
          let errs = errors c
              fails = failures c
          exitWith (codeGet errs fails)

codeGet errs fails
 | fails > 0       = ExitFailure 2
 | errs > 0        = ExitFailure 1
 | otherwise       = ExitSuccess
