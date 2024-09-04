module MathUtil where

-- Arithmetic:
add x y = x + y
inc1 = add 1

sub x y = x - y

mul x y = x * y

divi x y = x `div` y

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

{- Map -}
incrementList xs = map inc1 xs
-- shorthand:
-- incrementList = map inc1
squareList xs = map (\x -> x * x) xs
-- shorthand:
-- squareList = map (\x -> x * x)


{- Filter -}


{- Fold -}