module BMO where

-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [a] = a
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast xs = (head . tail . reverse) xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] index = error "Empty list"
elementAt xs 0 = error "No zeroth element"
elementAt xs index = head $ drop (index-1) xs

elementAt2 :: [a] -> Int -> a
elementAt2 [] index = error "Index out of range"
elementAt2 list 1 = head list
elementAt2 list index = elementAt2 (tail list) (index - 1)

-- Problem 4
myLength :: [a] -> Int
myLength xs =
  let
    inner :: [a] -> Int -> Int
    inner [] len = len
    inner xi len = inner (tail xi) (len + 1)
  in
   inner xs 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse xs =
  let
    inner :: [a] -> [a] -> [a]
    inner [] dest = dest
    inner (a:src) dest = inner src (a:dest)
  in
    inner xs []
