-- https://wiki.haskell.org/99_questions/1_to_10
module Lists where

import Control.Arrow ((&&&))
import Data.List (foldl', unfoldr)

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

-- Problem 11

data Grouping a = Single a | Multiple Int a
                  deriving Show

encodeModified :: (Eq a) => [a] -> [Grouping a]
encodeModified = map toGrouping . encode'
  where toGrouping (n, x) = if n == 1 then Single x else Multiple n x

-- Problem 12

decodeModified :: [Grouping a] -> [a]
decodeModified []                = []
decodeModified (Single x:xs)     = x : decodeModified xs
decodeModified (Multiple n x:xs) = replicate n x ++ decodeModified xs

decodeModified' :: [Grouping a] -> [a]
decodeModified' = (>>= expand)
  where expand (Single x)     = [x]
        expand (Multiple n x) = replicate n x

-- Problem 14

dupli :: [a] -> [a]
dupli = (>>= replicate 2)

-- Problem 15

repli :: [a] -> Int -> [a]
repli xs n = xs >>= replicate n

-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = loop n xs
  where loop _ []     = []
        loop 1 (_:xs) = loop n xs
        loop i (x:xs) = x : loop (pred i) xs

chunk n = unfoldr maybeSplitAt
  where maybeSplitAt [] = Nothing
        maybeSplitAt xs = Just $ splitAt n xs

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = chunk n xs >>= take (n - 1)

-- Problem 17

split :: [a] -> Int -> ([a], [a])
split xs n = loop n [] xs
  where loop _ acc []     = (acc, [])
        loop 0 acc xs     = (acc, xs)
        loop i acc (x:xs) = loop (pred i) (acc ++ [x]) xs

-- Problem 18

slice :: [a] -> Int -> Int -> [a]
slice xs lower upper = take (upper - lower + 1) $ drop (lower - 1) xs

-- Problem 19

rotate :: [a] -> Int -> [a]
rotate xs n
  | length xs < n = rotate xs (mod n $ length xs)
  | otherwise     = right ++ left
  where (left, right) = splitAt n xs

-- Problem 20

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n xs = (x, left ++ right)
  where (left, xAndRight) = splitAt (n - 1) xs
        (maybeX, right) = splitAt 1 xAndRight
        x = if null maybeX then Nothing else Just $ head maybeX
