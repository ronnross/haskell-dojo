module Problems where

import Data.List (reverse, head, last)

myLast :: [a] -> a
myLast [] = error "Empty"
myLast [a] = a
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "Empty"
myButLast [a] = error "List needs more than 1 value"
myButLast (x:xs) = if (length xs) == 1 then
                      x
                   else
                      myButLast xs


elementAt :: [a] -> Int -> a
elementAt [] xs = error "Empty list"
elementAt x xs = if (length x) < xs then
                      error "Index out of bounds"
                   else
                        x!!(xs - 1)

elementAt' :: [a] -> Int -> a
elementAt' [] xs = error "Empty list"
elementAt' x 1 = head x
elementAt' x' xs = elementAt' (tail x')  (xs - 1)

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

