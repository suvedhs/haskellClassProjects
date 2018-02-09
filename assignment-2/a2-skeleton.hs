
-- This is head comment (comment should be preceded by two dashes)
-- Assignment 2, CSCE 314 
-- Student Name: PUT YOUR NAME HERE
-- UIN: PUT YOUR UIN HERE
-- (Acknowledge any help received here)

module Main where

import Test.HUnit
import System.Exit

-- Problem 2
fibonacci :: Int -> Int
fibonacci = undefined

-- Problem 3
myProduct :: [Integer] -> Integer
myProduct = undefined

-- Problem 4
flatten :: [[a]] -> [a]
flatten = undefined

-- Problem 5
myLength :: [a] -> Int
myLength = undefined

-- Problem 6
quicksort :: Ord t => [t] -> [t]
quicksort = undefined

-- Problem 7
isElement :: Eq a => a -> [a] -> Bool
isElement = undefined





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
