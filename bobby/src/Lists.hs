-- https://wiki.haskell.org/99_questions/1_to_10
module Lists where

import Control.Arrow ((&&&))
import Data.List (foldl')

myLast :: [a] -> a
myLast []     = undefined
myLast [a]    = a
myLast (_:as) = myLast as

myButLast :: [a] -> a
myButLast = head . tail . reverse

elementAt :: [a] -> Int -> a
elementAt []     _ = undefined
elementAt (x:xs) n
  | n == 0         = undefined
  | n == 1         = x
  | otherwise      = elementAt xs $ pred n

elementAt' :: [a] -> Int -> a
elementAt' xs n = head $ drop (pred n) xs

myLength :: [a] -> Int
myLength xs = loop xs 0
  where loop []     n = n
        loop (x:xs) n = loop xs (succ n)

myLength' :: [a] -> Int
myLength' = foldl' (\n _ -> succ n) 0

myReverse :: [a] -> [a]
myReverse xs = loop xs []
  where loop []     acc = acc
        loop (x:xs) acc = loop xs (x:acc)

myReverse' :: [a] -> [a]
myReverse' = foldl' (flip (:)) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' = (==) <$> id <*> myReverse

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)     = [x]
flatten (List lists) = foldr (\list acc -> flatten list ++ acc) [] lists

compress :: (Eq a) => [a] -> [a]
compress []     = []
compress (x:xs) = x : loop x xs
  where loop _     []              = []
        loop prior xs@(first:rest)
          | prior == first         = loop prior rest
          | otherwise              = compress xs

compress' :: (Eq a) => [a] -> [a]
compress' []     = []
compress' (x:xs) = x : compress' next
  where next = dropWhile (== x) xs

pack :: (Eq a) => [a] -> [[a]]
pack []       = []
pack xs@(x:_) = block : pack rest
  where (block, rest) = span (== x) xs

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map toPair . pack
  where toPair xs = (length xs, head xs)

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = map (length &&& head) . pack
