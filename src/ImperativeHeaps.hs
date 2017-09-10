
module ImperativeHeaps where


import qualified Data.Vector as V

-- convenient ! for 1-index based arrays instead of 0-based array
(!) :: V.Vector a -> Int -> a
v ! i = v V.! (i - 1)

swap :: Ord a => Int -> Int -> V.Vector a -> V.Vector a
swap i j heap  = heap V.// [(i-1, heap ! j), (j-1, heap ! i)]

siftup :: Ord a => Int -> V.Vector a -> V.Vector a
siftup i heap = let j = div i 2
                    in if i == 1 || heap ! j <= heap ! i
                       then heap else siftup j $ swap i j heap

siftup' :: Ord a => Int -> V.Vector a -> V.Vector a
siftup' i heap | i == 1 || heap ! j <= heap ! i = heap
               | otherwise = siftup j $ swap i j heap
               where j = div i 2
               
