double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

a = b + c
    where
      b = 1
      c = 2
d = a * 2

n = a `div` length xs
     where
          a = 10
          xs = [1,2,3,4,5]

y = 1
myproduct (x:xs) = y
    where
        y = y * x

-- howdy ags groovy suvy is about to use this file to practice making functions
-- for the haskell midterm!! after this we are fucking done with haskell!!
-- ngl it's a beautiful language and I'll definitely be looking into potential
-- projects I can use haskell for because it's fucking great even if it's strict
-- as fuck. I think if i can actually get proficient at this language then I'll
-- have a good time.

-- Produce a list from 100 to 200 whose remainder when divided by 5 is 2
myList [] = []
myList (x:xs) = if x `mod` 5 == 2 then x : myList xs else myList xs

myListHundred = myList [100..200]

myListHundredGood = [x | x <- [100..200], x `mod` 5 == 2]

yesno xs = [if x > 10 then "YES" else "NO" | x <- xs, odd x]

long_list = [ x | x <- [1..], x /= 17 && x /= 19 && x /= 13 && x /= 11]
