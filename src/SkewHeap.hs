module SkewHeap(
               SkewHeap,
               union,
               singleton,
               extractMin,
               insert
               ) where

-- Skew Heap
data SkewHeap a = Empty | SkewNode a (SkewHeap a) (SkewHeap a)
                          deriving Show

union :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
union heap1@(SkewNode x1 l1 r1) heap2@(SkewNode x2 l2 r2)
  | x1 <= x2 = SkewNode x1 (heap2 `union` r1) l1
  | otherwise = SkewNode x2 (heap1 `union` r2) l2
union Empty heap = heap
union heap Empty = heap

singleton :: Ord a => a -> SkewHeap a
singleton a = SkewNode a Empty Empty

extractMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty = Nothing
extractMin (SkewNode x l r) = Just (x, (l `union` r))

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert a h = singleton a `union` h

