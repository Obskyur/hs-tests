{- Tristan Miller -}
{- SID: 11820319 -}

{- 1. sumEvens -}
sumEvens :: [Int] -> Int
sumEvens [] = 0 -- Base Case
sumEvens (x : xs) =
  -- Recursive Case
  if x `mod` 2 == 0
    then x + sumEvens xs
    else sumEvens xs

sumEvens' :: [Int] -> Int
sumEvens' x = foldr (+) 0 (filter even x) -- filtr / foldr

sumEvens'' :: [Int] -> Int
sumEvens'' x = foldr (+) 0 [x | x <- x, x `mod` 2 == 0] -- List Comprehension

{- 2. findValue -}
findValue :: (Eq a) => a -> [(a, b)] -> Maybe b -- a must be an instance of Eq for comparison
findValue _ [] = Nothing -- Base Case
findValue a ((x, y) : xs) =
  -- Recursive Case
  if a == x -- check if key x matches a
    then Just y -- return value y
    else findValue a xs -- else try again with the rest of the list

{- 3. mapMaybe -}
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = [] -- Base Case
mapMaybe f (x : xs) =
  -- Recursive Case
  case f x of -- check if f x returns Just y or Nothing
    Just y -> y : mapMaybe f xs -- if f x returns Just y, add y to the front of the list
    Nothing -> mapMaybe f xs -- else continue with the rest of the list