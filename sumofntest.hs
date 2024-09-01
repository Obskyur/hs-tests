module SumOfNTest where

import SumOfN
import Test.HUnit

addTest = TestCase (assertEqual "test add" 2 (add 1 1))

sumOfNTest1 = TestCase (assertEqual "test SumOfN 4" 10 (sumOfN 4))

sumOfNTest2 = TestCase (assertEqual "test SumOfN 0" 0 (sumOfN 0))

sumOfNTest3 = TestCase (assertEqual "test SumOfN 10" 55 (sumOfN 10))

tests = TestList [addTest, sumOfNTest1, sumOfNTest2, sumOfNTest3]

run = runTestTT tests
