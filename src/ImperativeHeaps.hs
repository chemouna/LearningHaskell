
module ImperativeHeaps where


import qualified Data.Vector as V

-- convenient ! for 1-index based arrays instead of 0-based array
(!) :: V.Vector a -> Int -> a
v ! i = v V.! (i - 1)

swap :: Ord a => Int -> Int -> V.Vector a -> V.Vector a
swap i j heap  = heap V.// [(i-1, heap ! j), (j-1, heap ! i)]


