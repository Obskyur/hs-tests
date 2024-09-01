module SumOfN
where

{- Fx Add: -}
  add x y = x + y

{- Fx sumOfN: -}
  sumOfN 0 = 0
  sumOfN n = n + sumOfN(n-1)

{- main = do
  let res = add 1 2 -}
