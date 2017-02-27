module Anthony where

import Data.List (reverse, head, tail, length, init, foldl')

-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty"
myButLast [x, _] = x
myButLast (x:xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) y = elementAt xs (y - 1)

-- Problem 4
myLength :: [a] -> Int
myLength xs = length xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse xs = reverse xs

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs
