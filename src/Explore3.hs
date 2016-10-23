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

-- 3 (find k"th element of a list)
element_at xs n = xs !! n
prop_3a xs n = (n < length xs && n >= 0) ==> element_at xs (n :: Int) == (xs !! n :: Int)

prop_3aa (NonEmpty xs) n = forAll (choose (0, length xs - 1)) $ \i ->
  element_at xs i == xs !! i

prop_3ab xs n = n >= 0 ==> forAll (listLongerThan n) $ \xs ->
  element_at xs n == xs !! n

listLongerThan :: Int -> Gen [Int]
listLongerThan n = replicateM (n + 1) arbitrary

-- listLongetThan generates only lists with lengths n + 1 -> let's improve it
prop_3ac = forAll smallNumber $ \n ->
           forAll (listLongerThan2 n) $ \xs ->
           element_at xs n == xs !! n

smallNumber :: Gen Int
smallNumber = fmap (`mod` 100) arbitrary

listLongerThan2 :: Int -> Gen [Int] 
listLongerThan2 n = do
  y <- fmap (+1) smallNumber
  replicateM (n+y) arbitrary

-- another way to do it
prop_3ad xs n = n >= 0 ==> length xs > n ==> element_at xs n == xs !! n

