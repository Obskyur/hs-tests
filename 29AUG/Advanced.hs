class Describable a where
  describe :: a -> String

data Currency = USD Double | INR Double

instance Describable Currency where
  describe (USD amount) = "United States Dollars: " ++ show amount
  describe (INR amount) = "Indian Rupees: " ++ show amount

instance Describable Bool where
  describe True = "This is True"
  describe False = "This is False"

dec :: Int -> Int
dec x = x - 1

inc :: Int -> Double
inc x = 1.0 + fromIntegral x

{- Functor: -}

-- equivalent to fmap:
myCompose1 :: (a -> b) -> Maybe a -> Maybe b -- function from normal univ, input from maybe universe
myCompose1 fn mx = case mx of
  Nothing -> Nothing
  Just val -> Just (fn val)

minc = Just inc

mypure x = Just x

-- equivalent to <*> (applicative operator):
myCompose2 :: Maybe (a -> b) -> Maybe a -> Maybe b -- function from maybe univ, input from maybe universe
myCompose2 mfn mx = case mfn of
  Nothing -> Nothing
  Just fn -> myCompose1 fn mx

myCompose1_1 :: (a -> b) -> Maybe a -> Maybe b
myCompose1_1 fn mx = myCompose2 (mypure fn) mx

-- 
recip' :: Int -> Maybe Double
recip' 0 = Nothing
recip' x = Just $ 1.0 / fromIntegral x

pprint :: Double -> Maybe String
pprint x = if x < 0.0
  then Nothing
  else Just $ "Value is: " ++ show x

-- equivalent to >>= (monadic operator):
myCompose3 :: Maybe a -> (a -> Maybe b) -> Maybe b
myCompose3 mx fnb1 = case mx of 
  Nothing -> Nothing
  Just x -> fnb1 x

-- applies decrement to an input functor, and decrements by y:
ndec mx y = fmap (\x -> x - y) mx