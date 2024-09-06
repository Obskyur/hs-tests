module MathUtil where

import Debug.Trace

-- Arithmetic:
add x y = x + y
inc1 = add 1

sub x y = x - y

mul x y = x * y


divi x y = x `div` y

even' x = x `mod` 2 == 0

-- List Length:
len1 [] = 0
len1 (x : xs) = 1 + len1 xs

len2 xs = if xs == [] then 0 else 1 + len2 (tail xs)

len3 xs
  | xs == [] = 0
  | otherwise = 1 + len3 (tail xs)

len4 xs = case xs of
  [] -> 0
  (_:ys) -> 1 + len4 ys

listConcat xs ys = xs ++ ys

{- Map -}
incrementList xs = map inc1 xs
-- shorthand:
-- incrementList = map inc1
squareList xs = map (\x -> x * x) xs
-- shorthand:
-- squareList = map (\x -> x * x)

{- Filter -}
evenOnly xs = filter even' xs

{- Fold -}
sumList xs = foldr (+) 0 xs

sumList' xs = foldl (+) 0 xs

incrementList' xs = foldr (\x acc -> inc1 x : acc) [] xs

{- Factorial -}
fac n
  | n == 0 = 1
  | otherwise = n * fac (n - 1)

mul1 x y | trace ("mul1 called with " ++ show x ++ " " ++ show y) False = undefined
mul1 0 _ = 0
mul1 _ 0 = 0
mul1 1 x = x
mul1 x 1 = x
mul1 x y = x + mul1 x (y - 1)

myFindIndexHelper (x: xs) n ele | trace ("x and n: " ++ show x ++ " " ++ show n) False = undefined
myFindIndexHelper [] _ _ = -1
myFindIndexHelper (x : xs) n ele = 
  if ele == x then n 
  else myFindIndexHelper xs (n + 1) ele
myFindIndex xs ele = myFindIndexHelper xs 0 ele

-- Write findMaxElement function that finds the maximum element in a list.
findMaxElementHelper [] maxEle = maxEle
findMaxElementHelper (x : xs) maxEle
  | x > maxEle = findMaxElementHelper xs x
  | otherwise = findMaxElementHelper xs maxEle

findMaxElement [] = -1
findMaxElement (x : xs) = findMaxElementHelper xs x

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myTake _ [] = []
myTake x (y:ys)
  | x <= 0 = []
  | otherwise = y : myTake (x - 1) ys