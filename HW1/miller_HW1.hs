{- 
  Tristan Miller
  SID: 11820319
 -}

module HW1 where

{- 1. Custom List Operations -}
-- a. dropList
dropList :: Int -> [a] -> [a] -- dropList takes an Int and a List and returns a List
dropList _ [] = []  -- Base case: empty list
dropList n xs    -- Has two cases: n is less than or equal to 0 or n is greater than 0
  | n <= 0 = xs   -- Base case: n is less than or equal to 0
  | otherwise = dropList (n - 1) (tail xs) -- Recursive case: decrement n, drop the first element

-- b. splitAtIndex
splitAtIndex :: Int -> [a] -> ([a], [a])    -- splitAtIndex takes an Int and a List and returns a tuple of two Lists
splitAtIndex n xs = (take n xs, dropList n xs)    -- Use take to get the first n elements and dropList to get the rest

-- c. concatLists
concatLists :: [a] -> [a] -> [a]    -- concatLists takes two Lists and returns a List
concatLists [] ys = ys  -- Base case: first list is empty
concatLists xs [] = xs  -- Base case: second list is empty
concatLists (x:xs) ys = x : concatLists xs ys -- Recursive case: add the first element of the first list to the second list

-- d. interleaveLists
interleaveLists :: [a] -> [a] -> [a]    -- interleaveLists takes two Lists and returns a List
interleaveLists [] ys = ys  -- Base case: first list is empty
interleaveLists xs [] = xs  -- Base case: second list is empty
interleaveLists (x:xs) (y:ys) = x : y : interleaveLists xs ys -- Recursive case: add the first element of each list, then recurse

{- 2. Merging Sorted Lists -}
-- a. mergeAscending
mergeAscending :: Ord a => [a] -> [a] -> [a]    -- mergeAscending takes two Ord Lists and returns an Ord List
mergeAscending [] ys = ys  -- Base case: first list is empty
mergeAscending xs [] = xs  -- Base case: second list is empty
mergeAscending (x:xs) (y:ys)    -- Recursive case: compare the first elements of each list
  | x <= y = x : mergeAscending xs (y:ys)    -- If x is less than or equal to y, prepend x to the result and recurse
  | otherwise = y : mergeAscending (x:xs) ys    -- If y is less than x, prepend y to the result and recurse

-- b. mergeDescending
mergeDescending :: Ord a => [a] -> [a] -> [a]    -- mergeDescending takes two Ord Lists and returns an Ord List
mergeDescending [] ys = ys  -- Base case: first list is empty
mergeDescending xs [] = xs  -- Base case: second list is empty
mergeDescending (x:xs) (y:ys)    -- Recursive case: compare the first elements of each list
  | x >= y = x : mergeDescending xs (y:ys)    -- If x is greater than or equal to y, prepend x to the result and recurse
  | otherwise = y : mergeDescending (x:xs) ys    -- If y is greater than x, prepend y to the result and recurse

{- Sorting Algorithms -}
-- a. mergeSort
mergeSort :: Ord a => [a] -> [a]    -- mergeSort takes a List and returns an Ord List
mergeSort [] = []    -- Base case: empty list
mergeSort [x] = [x]    -- Base case: single element list
-- Use our mergeAscending and splitAtIndex functions to mergeSort the list
mergeSort xs = mergeAscending (mergeSort left) (mergeSort right)
  where (left, right) = splitAtIndex index xs    -- Split the list in half
        index = length xs `div` 2    -- Find the midpoint of the list

-- b. insertionSort
insertionSort :: Ord a => [a] -> [a]    -- insertionSort takes an Ord List and returns an Ord List
insertionSort [] = []   -- Base case: empty list
insertionSort [x] = [x]    -- Base case: single element list
insertionSort (x:xs) = insert x (insertionSort xs)    -- Insert the first element into the sorted list
  where insert x [] = [x]    -- Base case: empty list
        insert x (y:ys)    -- Recursive case: compare x to y
          | x <= y = x : y : ys    -- If x is less than or equal to y, insert x before y
          | otherwise = y : insert x ys    -- If x is greater than y, insert x into the rest of the list