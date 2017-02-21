module BMO where

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [a] = a
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast xs = (head . tail . reverse) xs

elementAt :: [a] -> Int -> a
elementAt [] index = error "Empty list"
elementAt xs 0 = error "No zeroth element"
-- elementAt xs index = head $ drop (index-1) xs
elementAt xs index =
  let
    inner :: [a] -> Int -> a
    inner list 0 = head list
    inner [] index = error "Index out of range"
    inner list index = inner (tail list) (index - 1)
  in
    inner xs (index - 1)

myLength :: [a] -> Int
myLength xs =
  let
    inner :: [a] -> Int -> Int
    inner [] len = len
    inner xi len = inner (tail xi) (len + 1)
  in
   inner xs 0

myReverse :: [a] -> [a]
myReverse xs =
  let
    inner :: [a] -> [a] -> [a]
    inner [] dest = dest
    inner (a:src) dest = inner src (a:dest)
  in
    inner xs []
