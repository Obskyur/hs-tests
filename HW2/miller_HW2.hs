{- Tristan Miller -}
{- SID: 11820319 -}

module HW2 where

{- 1. Data Types -}
data Currency = USD Double | INR Double
  deriving (Show)

-- Implementing Eq for Currency for testing purposes
instance Eq Currency where
  (USD v1) == (USD v2) = v1 == v2
  (INR v1) == (INR v2) = v1 == v2
  (USD v1) == (INR v2) = v1 == (v2 / 82)
  (INR v1) == (USD v2) = (v1 / 82) == v2

-- Helper function to convert between USD and INR
convert :: Currency -> Currency
convert (USD v) = INR (v * 82) -- 1 USD = 82 INR
convert (INR v) = USD (v / 82) -- 1 INR = 0.012 USD

-- Helper function to get the value of a currency in USD
toUSD :: Currency -> Currency
toUSD (USD v) = USD v
toUSD (INR v) = convert (INR v)

-- Helper function to get the value of a currency in INR
toINR :: Currency -> Currency
toINR (INR v) = INR v
toINR (USD v) = convert (USD v)

{- 2. Binary Search Tree -}
data BST a = EmptyNode | Node a (BST a) (BST a)
  deriving (Show, Eq)

-- Helper function to get the value of a currency in USD for comparisons
valueInUSD :: Currency -> Double
valueInUSD (USD v) = v
valueInUSD (INR v) = v / 82

-- Insert a currency into the BST
insert :: Currency -> BST Currency -> BST Currency
insert c EmptyNode = Node c EmptyNode EmptyNode
insert c (Node c' left right)
  | valueInUSD c < valueInUSD c' = Node c' (insert c left) right
  | otherwise = Node c' left (insert c right)

{- 3. Searching for a Value -}
search :: Currency -> BST Currency -> Bool
search _ EmptyNode = False -- Base case: empty tree
search c (Node c' left right) -- Search for a currency in the BST using DFS
  | valueInUSD c == valueInUSD c' = True -- Found the value
  | valueInUSD c < valueInUSD c' = search c left -- Search left subtree
  | otherwise = search c right -- Search right subtree

{- 4. Summing Currency Values -}
totalCurrency :: BST Currency -> Double
totalCurrency EmptyNode = 0 -- Base case: empty tree
totalCurrency (Node c left right) =
  -- Sum the values of the currencies in the BST using DFS
  valueInUSD c + totalCurrency left + totalCurrency right

{- 5. Converting the Tree -}
convertTree :: (a -> a) -> BST a -> BST a
convertTree _ EmptyNode = EmptyNode -- Base case: empty tree
convertTree f (Node c left right) =
  -- Convert the values of the currencies in the BST using DFS
  Node (f c) (convertTree f left) (convertTree f right)

{- 6. BST as a Functor and Use fmap -}
instance Functor BST where
  fmap _ EmptyNode = EmptyNode
  fmap f (Node c left right) = Node (f c) (fmap f left) (fmap f right)

-- New convertTree function using fmap:
convertTree' :: (a -> a) -> BST a -> BST a
convertTree' = fmap

{- 7. Testing -}
-- Test data
testData :: BST Currency
testData = Node (USD 3) (Node (USD 2) EmptyNode EmptyNode) (Node (USD 5) EmptyNode EmptyNode)

-- Test the insert function
testInsert :: Bool
testInsert = insert (USD 4) testData == Node (USD 3) (Node (USD 2) EmptyNode EmptyNode) (Node (USD 5) (Node (USD 4) EmptyNode EmptyNode) EmptyNode)

-- Test the search function
testSearch :: Bool
testSearch = search (USD 2) testData == True

-- Test the totalCurrency function
testTotalCurrency :: Bool
testTotalCurrency = totalCurrency testData == 10

-- Test the convertTree function
testConvertTree :: Bool
testConvertTree = convertTree toINR testData == Node (INR (3*82)) (Node (INR (2*82)) EmptyNode EmptyNode) (Node (INR (5*82)) EmptyNode EmptyNode)

-- Test the convertTree' function
testConvertTree' :: Bool
testConvertTree' = convertTree' toINR testData == Node (INR (3*82)) (Node (INR (2*82)) EmptyNode EmptyNode) (Node (INR (5*82)) EmptyNode EmptyNode)

-- Run all tests
runTests :: IO ()
runTests = do
  putStrLn "Running tests..."
  putStrLn $ "testInsert: " ++ show testInsert
  putStrLn $ "testSearch: " ++ show testSearch
  putStrLn $ "testTotalCurrency: " ++ show testTotalCurrency
  putStrLn $ "testConvertTree: " ++ show testConvertTree
  putStrLn $ "testConvertTree': " ++ show testConvertTree'
  putStrLn "Tests complete."
