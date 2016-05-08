module Learning.Monoid where

import Data.Monoid

newtype MyProduct a = MyProduct { getProduct :: a } deriving (Eq, Ord, Show, Read, Bounded)

instance Num a => Monoid (MyProduct a) where
  mempty = MyProduct 1
  MyProduct x `mappend` MyProduct y = MyProduct (x * y)