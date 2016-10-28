module PairingHeap(
                 PairingHeap,
                 union,
                 extractMin,
                 insert,
                 singleton
                 ) where

import Data.Foldable

data PairingHeap a = Empty | PairNode a [PairingHeap a]

union :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
union heap1@(PairNode x1 ts1) heap2@(PairNode x2 ts2)
  | x1 <= x2 = PairNode x1 (heap2 : ts1)
  | otherwise = PairNode x2 (heap1 : ts2)
union Empty heap = heap
union heap Empty = heap

extractMin :: Ord a => PairingHeap a -> Maybe (a, PairingHeap a)
extractMin (PairNode x ts) = Just (x, meldChildren ts)
  where meldChildren (t1:t2:ts) = (t1 `union` t2) `union` meldChildren ts
        meldChildren [t] = t
        meldChildren [] = Empty
extractMin Empty = Nothing

singleton :: Ord a => a -> PairingHeap a
singleton x = PairNode x []

peekMin :: Ord a => PairingHeap a -> Maybe a
peekMin h = fmap fst $ extractMin h

insert :: Ord a => a -> PairingHeap a -> PairingHeap a
insert a h = singleton a `union` h

insertMany :: (Foldable t, Ord a) => t a -> PairingHeap a -> PairingHeap a
insertMany as h = foldl (\h x -> insert x h) Empty as 
