-- Problem 1
myLast::[a]->a
myLast [x] = x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast::[a]->a
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

-- Problem 3
elementAt::[a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) p = elementAt xs (p-1)

-- Problem 4
myLength::[a] -> Int
myLength [] = 0
myLength (x:xs) = (myLength xs) + 1

-- Problem 5
myReverse::[a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
