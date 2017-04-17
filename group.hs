module Problems where

import Data.List (reverse, head, tail, length, init, map, concat)


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

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) =
  let
    loop :: (Eq b) => [b] -> b -> [[b]] -> [[b]]
    loop [] e p = p
    loop (x:xs) e (p:ps)
      | x == e     = loop xs x ((x:p):ps)
      | otherwise  = loop xs x ([x]:p:ps)
  in
    reverse $ loop xs x [[x]]

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs =
  let
    loop :: [[b]] -> [(Int, b)]
    loop [] = []
    loop (x:xs) = (length x, head x):(loop xs)
  in
    loop $ pack xs

-- Problem 11
data EncodedEl a = Single a | Multiple Int a deriving Show

encodeModified :: (Eq a) => [a] -> [EncodedEl a]
encodeModified =
  let
    encodeRun :: [a] -> EncodedEl a
    encodeRun [x] = Single x
    encodeRun xs = Multiple (length xs) (head xs)
  in
    map encodeRun . pack -- ["aaaa","b","cc","aa","d","eeee"]


-- Problem 12
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
decodeModified :: [EncodedEl a] -> [a]
decodeModified xs =
  let expand :: EncodedEl a -> [a]
      expand (Single x) = [x]
      expand (Multiple 0 x) = []
      expand (Multiple n x) = x : expand (Multiple (n-1) x)
  in
    concat $ map expand xs

