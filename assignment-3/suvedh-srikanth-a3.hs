
-- Assignment 3, CSCE-314
-- Section: 501
-- Student Name: Suvedh Srikanth
-- UIN: 325001057
-- (Acknowledge any help received here)

module Main where

import Test.HUnit
import System.Exit
import Data.List
import Data.Char


---- Part 1.
-- Problem 1
cutWhitespace = map(dropWhile(isSpace))

-- Problem 2

multListt = zipWith (zipWith (*))

---- Part 2.
-- Problem 3.1
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) = if (x < y) then x : merge xs (y:ys)
  else y : merge (x:xs) ys

-- problem 3.2
halves :: [a] -> ([a], [a])
halves xs = (take n xs, drop n xs) where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [z] = [z]
msort ns = merge (msort beginning) (msort end)
  where (beginning, end) = halves ns


-- Problem 4
multiply :: [Int] -> Int
multiply = foldr (*) 1

-- Problem 5
concatenate :: [String] -> String
concatenate = foldl (++) ""

-- Problem 6
concatenateAndUpcaseOddLengthStrings :: [String] -> String
concatenateAndUpcaseOddLengthStrings ns = concatenate (map (map toUpper) (filter (odd . length) ns))



---- Part 3.
data Tree a b = Branch b (Tree a b) (Tree a b)
              | Leaf a



-- Problem 7
preorder  :: (a -> c) -> (b -> c) -> Tree a b -> [c]
preorder map1 map2 (Leaf n) = [map1 n]
preorder map1 map2 (Branch b l r) = [map2 b] ++ (preorder map1 map2 l) ++ (preorder map1 map2 r)

inorder   :: (a -> c) -> (b -> c) -> Tree a b -> [c]
inorder map1 map2 (Leaf n) = [map1 n]
inorder map1 map2 (Branch b l r) = (inorder map1 map2 l) ++ [map2 b] ++ (inorder map1 map2 r)

---- Part 4.
data E = IntLit Int
       | BoolLit Bool
       | Plus E E    -- for addition
       | Mult E E    -- for multiplication
       | Equals E E
         deriving (Eq, Show)

-- Problem 8
toInt :: E -> Int
toInt (IntLit x) = x
toInt (BoolLit x) = if x == True then 1
  else 0

eval :: E -> E
eval (IntLit x) = IntLit x
eval (BoolLit x) = BoolLit x
eval (Plus x y) = IntLit ((+) (toInt (eval x)) (toInt (eval y)) )
eval (Mult x y) = IntLit ((*) (toInt (eval x)) (toInt (eval y)) )
eval (Equals x y) = BoolLit ((==) (toInt (eval x)) (toInt (eval y)) )


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
