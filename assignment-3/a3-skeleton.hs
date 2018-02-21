
-- Assignment 3, CSCE-314
-- Section: PUT YOUR SECTION (500 or 501) HERE
-- Student Name: PUT YOUR NAME HERE
-- UIN: PUT YOUR UIN HERE
-- (Acknowledge any help received here)

module Main where

import Test.HUnit
import System.Exit
import Data.List
import Data.Char


---- Part 1.
-- Problem 1

cutWhitespace = undefined

-- Problem 2

multListt = undefined

---- Part 2.
-- Problem 3.1
-- problem 3.2


-- Problem 4
multiply :: [Int] -> Int
multiply = undefined

-- Problem 5
concatenate :: [String] -> String     
concatenate = undefined

-- Problem 6
concatenateAndUpcaseOddLengthStrings :: [String] -> String
concatenateAndUpcaseOddLengthStrings = undefined



---- Part 3.
data Tree a b = Branch b (Tree a b) (Tree a b)
              | Leaf a


   
-- Problem 7
preorder  :: (a -> c) -> (b -> c) -> Tree a b -> [c]
preorder = undefined

inorder   :: (a -> c) -> (b -> c) -> Tree a b -> [c]
inorder = undefined

---- Part 4. 
data E = IntLit Int
       | BoolLit Bool
       | Plus E E    -- for addition
       | Mult E E    -- for multiplication
       | Equals E E
         deriving (Eq, Show)

-- Problem 8
eval :: E -> E
eval = undefined


mytree = Branch "A" 
           (Branch "B" 
              (Leaf 1) 
              (Leaf 2)) 
           (Leaf 3)

prog1 = Equals 
           (Plus (IntLit 1) (IntLit 9))
           (Mult
              (IntLit 5)
              (Plus (IntLit 1) (IntLit 1)))

prog2 = Equals
           (Equals
              (Mult (IntLit 4) (IntLit 2))
              (Plus (IntLit 5) (Mult (IntLit 2) (IntLit 1))))
           (Equals (BoolLit True) (BoolLit True))

    
myTestList =

  let te s e a = test $ assertEqual s e a
      tb s b = test $ assertBool s b
  in
    TestList [ 
        

       te "multiply" 10 (multiply [-2, -1, 5])
      
      , te "concatenate" "ABCD" (concatenate ["AB", "", "", "C", "D", ""])

      , te "concatenateAndUpcaseOddLengthStrings"
          "HERE'S AN EXAMPLE" (concatenateAndUpcaseOddLengthStrings ["here's ", "an ", "a ", "example"])

      

      , te "preorder" "AB123" (concatenate (preorder show id mytree))
      , te "inorder" "1B2A3" (concatenate (inorder show id mytree))

      , te "eval1" (BoolLit True) (eval prog1)
      , te "eval2" (BoolLit False) (eval prog2)
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
