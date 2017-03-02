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

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- Problem 7
-- data NestedList a = Elem a | List [NestedList a]
-- flatten :: NestedList a -> [a]
-- flatten List [] = []
-- flatten List [x] = []
-- flatten Elem x = [x]

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = 
    let (next:nextTail) = compress xs
    in if next == x
        then [next] ++ nextTail
        else [x, next] ++ nextTail

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack (x:xs) = 
    let ((next:nextTail):rest) = pack xs
    in if next == x
        then [[x, next] ++ nextTail] ++ rest
        else [[x], [next] ++ nextTail] ++ rest

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1, x)]
encode (x:xs) = 
    let ((count, next):rest) = encode xs
    in if next == x
        then [(count + 1, next)] ++ rest
        else [(1, x), (count, next)] ++ rest