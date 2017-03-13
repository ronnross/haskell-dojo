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

-- Concentric ring grid (has type signature errors :/ )
generateGrid :: (Num a) => (Eq a) => a -> a -> [Char]
generateGrid x y = myGrid x y x y

myGrid :: (Num a) => (Eq a) => a -> a -> a -> a -> [Char]
myGrid x 1 w h = myRow x 1 w h
myGrid x y w h = (myGrid x (y - 1) w h) ++ myRow x y w h ++ "\n"

myRow :: a -> a -> a -> a -> [Char]
myRow 1 y w h = minimum 1 y w h
myRow x y w h = myRow (x - 1) y w h ++ minimum x y w h
