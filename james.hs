module JamesProblems where

-- problem 1
myLast :: [a] -> a
myLast [] = error "empty List"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

--problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast (x:[]) = error "list not long enough"
myButLast (x:xs)
  | length xs == 1 = x
  | otherwise = myButLast xs

-- problem 3
elementAt ::  [a] -> Int -> a
elementAt [] _ = error "Empty List"
elementAt xs i
  | length xs < i = error "index out of bounds"
  | i < 1 = error "index out of bounds"
  | otherwise = let drop' 1 (x:xs) = xs
                    drop' n (x:xs) = drop (n - 1) xs
                    head' (x:xs) = x
                in head' $ drop (i-1) xs

-- problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:[]) = [x]
myReverse xs =
  let foo ([],ys) = ys
      foo ((z:zs), ys) = foo (zs, z:ys)
  in foo (xs,[])

-- problem 6
isPalindrome :: (Show a) => a -> Bool
isPalindrome n = show n == (myReverse . show) n

-- problem 7

data NestedList a = Elem a | List [NestedList a]

instance Foldable NestedList where
  foldMap f (Elem a) = f a
  foldMap f (List [a]) = foldMap f a

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List xs) = foldr (\nl ys -> flatten nl ++ ys ) [] xs


-- problem 8
compress ::Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:xs)
  | head xs == x = compress xs
  | otherwise = x : compress xs

-- problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack (x:[]) = [[x]]
pack xs =
  let foo ys [] = [ys]
      foo [] (x:xs) = foo [x] xs
      foo (y:ys) (x:xs)
        | y == x = foo (y:x:ys) xs
        | otherwise = (y:ys) : foo [] (x:xs)
  in foo [] xs


-- problem 10
encode :: Eq a => [a] -> [(Int,a)]
encode = map (\ys -> (length ys, head ys)) . pack

--problem 11
data Encoding a = Single a | Multiple Int a deriving Show

encode' ::  Eq a => [a] -> [Encoding a]
encode' = map foo . encode
  where foo (1,x) = Single x
        foo (n,x) = Multiple n x

-- problem 12
decode :: [Encoding a] -> [a]
decode =
  let foo (Single x) = [x]
      foo (Multiple n x) = take n $ repeat x
  in foldr1 (++)  . map foo


-- problem 13
directEncode :: Eq a => [a] -> [Encoding a]
directEncode [] = []
directEncode (x:xs) =
  let a = x: takeWhile (==x) xs
      b = directEncode $ dropWhile (==x) xs
      foo (y:[]) = Single y
      foo ys = Multiple (length ys)  (head ys)

  in foo a : b

-- problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x: dupli xs


-- problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = ns ++ repli xs n
  where ns = take n $ repeat x

-- problem 16
drop' :: [a] -> Int -> [a]
drop' [] _ = []
drop' xs n = take (n-1) xs ++ drop' (drop n xs) n

-- problem 17
split :: [a] -> Int -> [[a]]
split [] _ = [[],[]]
split xs n = take n xs : drop n xs : []

-- problem 18
slice ::  Int -> Int -> [a] -> [a]
slice i k = drop (i-1) . take k

--problem 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:[]) _ = [x]
rotate (x:xs) 1 = xs ++ [x]
rotate (x:xs) n
  | n < 0 = let l = length (x:xs) + n
            in rotate (x:xs) l
  | otherwise = let ys = xs ++ [x]
                in rotate ys (n-1)

-- problem 20
removeAt :: Int -> [a] -> [a]
removeAt n xs
  | n < 1 = error "out of bounds"
  | otherwise = take (n-1) xs ++  drop n xs
