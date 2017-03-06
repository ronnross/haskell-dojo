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

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse(xs)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a  -> [a]
flatten (Elem a) = [a]
flatten (List xs) = 
  let
    loop :: [NestedList a] -> [a]
    loop [] = []
    loop (y:ys) = flatten y ++ loop ys
  in
    loop xs 

flatten' :: NestedList a  -> [a]
flatten' (Elem a) = [a]
flatten' (List xs) = 
  let
    loop :: [NestedList b] -> [b] -> [b]
    loop [] flat = flat
    loop (y:ys) flat = loop ys (flat ++ (flatten' y))
  in
    loop xs []

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) =
  let
    loop :: (Eq a) => [a] -> a -> [a]-> [a]
    loop [] elm com = com
    loop (y:ys) elm com
      | y == elm   = loop ys elm com
      | otherwise  = loop ys y (com ++ [y])
  in
    loop xs x [x]

compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' (x:xs) =
  let
    loop :: (Eq a) => [a] -> a -> [a]-> [a]
    loop [] elm com = com
    loop (y:ys) elm com =
      if y == elm then
        loop ys elm com
      else
        loop ys y (com ++ [y])
  in
    loop xs x [x]

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

-- Box Rings
printRect :: Integer -> Integer -> String
printRect x y =
  let
    maxX = fromIntegral (x + 1)/2.0
    maxY = fromIntegral (y + 1)/2.0
    entryWidth = (length . show) $ floor $ min maxX maxY
    
    lpad :: String -> String
    lpad str = replicate (entryWidth - length str) '0' ++ str

    getValue :: Integer -> Integer -> String
    getValue x y =
      let
        xVal = maxX - (abs $ fromIntegral x - maxX)
        yVal = maxY - (abs $ fromIntegral y - maxY)
      in
        (lpad . show . floor) $ min xVal yVal
    
    getRow :: Integer -> Integer -> Integer -> [String]
    getRow x maxX y
      | x == maxX   = [getValue x y]
      | otherwise   = (getValue x y):(getRow (x + 1) maxX y)
    
    getRowStr :: Integer -> Integer -> Integer -> String
    getRowStr x maxX y = unwords $ getRow x maxX y

    loop :: Integer -> Integer -> Integer -> [String]
    loop maxX y maxY
      | y == maxY   = [(getRowStr 1 maxX y)]
      | otherwise   =  (getRowStr 1 maxX y):(loop maxX (y + 1) maxY)
  in
    unlines $ loop x 1 y

-- putStrLn $ printRect 30 30