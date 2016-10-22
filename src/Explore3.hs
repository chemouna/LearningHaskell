module Explore3 where

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

