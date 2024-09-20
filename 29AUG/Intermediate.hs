
{- 19 SEP -}
reciprocal 0 = Nothing
reciprocal x = Just $ 1 / x

recip' :: Double -> Either String Double
recip' 0 = Left "divide by 0"
recip' x = Right $ 1 / x

recipS :: Double -> Either String Double
recipS 0 = Left "divide by 0"
recipS x = Right $ 1 / (x ^ 2)

-- getmeval :: Double -> Either String Double
-- getmeval x = case recip2 x of
--   Left err -> "Error: " ++ err
--   Right val -> val

getmeval :: Double -> Either String Double
getmeval x = case recip' x of
  Left err -> Left $ "Error: " ++ err
  Right val -> Right val

sqr :: Double -> Double
sqr x = x * x

-- get recip of an input first, then square it
recipSqr :: Double -> Either String Double
recipSqr x = case recip' x of
  Left err -> Left $ "Error: " ++ err
  Right val -> Right $ sqr val

recipSS :: Double -> Either [Char] Double
recipSS x = case recipS x of
  Left err -> Left $ "Error: " ++ err
  Right val -> Right $ sqr val

myCompose :: (a -> Either String b) -> (b -> c) -> a -> Either String c
myCompose ef f x = case ef x of
  Left err -> Left $ "Error: " ++ err
  Right val -> Right $ f val

myCompose' :: (t1 -> Either [Char] t2) -> (t2 -> Either [Char] b) -> t1 -> Either [Char] b
myCompose' ef1 ef2 x = case ef1 x of
  Left err -> Left $ "Error: " ++ err
  Right val -> ef2 val

recipSS' = recip' `myCompose'` recipS

recipSqr2 :: Double -> Either String Double
recipSqr2 = recip' `myCompose` sqr
