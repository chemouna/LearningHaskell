module Explore3 where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.All

f :: Int -> Int -> Bool
f x y = True

prop_f :: Property
prop_f = forAll (choose (0, 10)) $ \x ->
         forAll (choose (10, 20)) $ \y ->
         f x y

prop_f' :: Property
prop_f' = forAll ((,) <$> r1 <*> r2) $ \(x,y) -> f x y
  where
    r1 = choose (0, 10)
    r2 = choose (10, 20)

-- or by defining our own new types
newtype R1 = R1 Int deriving Show
newtype R2 = R2 Int deriving Show

instance Arbitrary R1 where
  arbitrary = R1 <$> choose (0, 10)

instance Arbitrary R2 where
  arbitrary = R2 <$> choose (10, 20)

prop_f'' (R1 x) (R2 y) = f x y

