module Problems where

import Data.List (reverse, head, tail, length, init)


data Maybe' a = Nil | Resolve a deriving Show

myLast' :: [a] -> Maybe' a
myLast' [] = Nil
myLast' [x] = Resolve x
myLast' (_:xs) = myLast' xs

myLast :: [a] -> a
myLast [] = error "Empty"
myLast [a, _] = a
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "Empty dude do you even haskell"
myButLast xs = head $ tail $ reverse xs

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- data List a = Nil | Cons a (List a)

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) =
  if x == y then y : compress xs
  else x : y : compress xs
