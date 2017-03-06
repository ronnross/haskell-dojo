module Problems where

import Data.List (reverse, head, tail, length, init, foldl')
-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty"
myLast [a, _] = a
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty dude do you even haskell"
myButLast xs = head $ tail $ reverse xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt xs i = xs !! (i - 1)

-- alt
--elementAt' :: [a] -> Int -> a
elementAt' [] i = error "Empty"
elementAt' xs i =
  let loop (x:xs) idx i = if idx == i then
                            x
                          else
                            loop xs idx (i + 1)

  in loop xs i 1

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength xs =
  let loop (_:xs) i = if (null xs) then
                        i
                      else
                        loop xs (i + 1)
  in loop xs 1

-- implement with foldl'

-- Problem 5
myReverse :: [a] -> [a]
myReverse xs =
  let loop (x:xs) col = if (null xs) then
                        x:col
                      else
                        loop xs (x:col)
  in loop xs []

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs


-- Problem 7
--flatten :: a -> [a]
--flatten (Elem x ) = [x]
--flatten (List xs) =  foldr (++) [] $ map flatten xs

-- Problem 8
-- compress :: [a] -> [a]
compress xs =
  let compressor (x:xs) col =
                  if x == (head xs) then
                    compressor xs col
                  else
                    compressor xs (x:col)
  in
    compressor xs []
