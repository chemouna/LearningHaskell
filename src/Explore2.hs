
module Explore2 where

import Control.Monad
import Data.List
import Test.QuickCheck
import Test.QuickCheck.All

data Age = Age Int
         deriving Show

instance Arbitrary Age where
  arbitrary = Age `liftM` choose (0, 100)

type Name = String
data Person = Person Name Age
            deriving Show

instance Arbitrary Person where
  arbitrary = liftM2 Person arbitrary arbitrary

-- sample (arbitrary :: Gen Person)

prop_Square x = x^2 >= x
prop_cubeOfPositive (NonNegative x) = x^3 >= x


fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

smallNonNegativeIntegers = choose (0, 500)

prop_fibonacci = forAll smallNonNegativeIntegers $ \n ->
  let x = fibs !! n
      y = fibs !! (n + 1)
      z = fibs !! (n + 2)
  in x + y == z


  
