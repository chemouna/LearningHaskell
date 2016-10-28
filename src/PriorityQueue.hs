module PriorityQueue where

import qualified SkewHeap as SH

class PriorityQueue q where
  extractMin :: Ord a => q a -> Maybe (a, q a)

  union :: Ord a => q a -> q a -> q a

  singleton :: Ord a => a -> q a

  insert :: Ord a => a -> q a -> q a

instance PriorityQueue SH.SkewHeap where
  extractMin = SH.extractMin
  union = SH.union
  singleton = SH.singleton
  insert = SH.insert



