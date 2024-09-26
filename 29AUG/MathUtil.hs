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
  (_ : ys) -> 1 + len4 ys

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


data MightBe = Empty | Have Int
  deriving (Show)

-- myFindIndexHelper (x : xs) n ele | trace ("x and n: " ++ show x ++ " " ++ show n) False = undefined
myFindIndexHelper [] _ _ = Empty
myFindIndexHelper (x : xs) n ele =
  if ele == x
    then Have n
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
myReverse (x : xs) = myReverse xs ++ [x]

myTake _ [] = []
myTake x (y : ys)
  | x <= 0 = []
  | otherwise = y : myTake (x - 1) ys

{- List Ops -}
splitByConditionHelper [] _ res = res
splitByConditionHelper (x : xs) c (leftList, rightList) =
  if c x
    then splitByConditionHelper xs c (leftList ++ [x], rightList) -- less efficient due to list copying
    else splitByConditionHelper xs c (leftList, x : rightList) -- more efficient than leftList ++ [x]

splitByCondition xs c = splitByConditionHelper xs c ([], [])

pTriples =
  [ (x, y, z)
    | x <- [1 .. 100],
      y <- [1 .. 100],
      z <- [1 .. 100],
      x ^ 2 + y ^ 2 == z ^ 2
  ] -- Pythagorean triples

nestedList :: [[Int]]
nestedList = [[1 .. n] | n <- [1 .. 5]] -- [[1], [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs) -- infinite list of fibonacci numbers, use 'take 10 fibs' to get first 10 fibonacci numbers

numbers = [1..100]
odd' n | trace ("\n called: " ++ show n ++ "\n") False = undefined
odd' x = x `mod` 2 /= 0 -- == odd' x = mod x 2 /= 0
getNodds n = take n (filter odd' numbers)

{- Type Defining -}

-- newtype USD = USD Double
--   deriving (Show)
-- newtype EUR = EUR Double
--   deriving (Show)

data Currency = USD Double | EUR Double
  deriving (Show)
convert (USD v) = EUR (v * 1.1)
convert (EUR v) = USD (v / 1.1)

data Shape = Rectangle Float Float | Square Float

-- Using the Shape type
area :: Shape -> Float
area (Rectangle width height) = width * height
area (Square side) = side * side

-- Data Tree
data Tree = EmptyNode | Node Int Tree Tree
  deriving (Show)
tree = Node 1
  (Node 2 EmptyNode EmptyNode)
  (Node 3 EmptyNode EmptyNode)

treeDepth :: Tree -> Int
treeDepth EmptyNode = 0
treeDepth (Node _ leftSubTree rightSubTree) =
  1 + max
    (treeDepth leftSubTree)
    (treeDepth rightSubTree)

treeFind :: Tree -> Int -> Bool
treeFind EmptyNode _ = False
treeFind (Node value leftSubTree rightSubTree) target
  | value == target = True
  | treeFind leftSubTree target = True
  | otherwise = treeFind rightSubTree target