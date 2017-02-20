module Problems where

import Data.List (reverse, head, tail, length, init)
myLast :: [a] -> a
myLast [] = error "Empty"
myLast [a, _] = a
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "Empty dude do you even haskell"
myButLast xs = head $ tail $ reverse xs
length [1,2,3,4]
