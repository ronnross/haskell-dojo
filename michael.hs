module Homework where

problem3 :: [a] -> Int -> a
problem3 [] index = error "Can not get element from empty list"
problem3 array index = array !! (index-1)

problem4 :: [a] -> Int
problem4 list = length list

problem5 :: [a] -> [a]
problem5 list = reverse list
