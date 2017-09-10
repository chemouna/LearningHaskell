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

siftdown :: Ord a => Int -> V.Vector a -> V.Vector a
siftdown n h = f 1 h where
  f i heap = if 2 * i > n || heap ! i <= c
             then heap
             else f j $ swap i j heap
    where (c, j) = minimum [(heap ! x, x) | x <- [2 * i, 2 * i + 1], x <= n]


hsort :: Ord a => V.Vector a -> V.Vector a
hsort heap = foldr (\i -> siftdown (i - 1) . swap 1 i) (foldl (flip siftup) heap [2..V.length heap]) [2..V.length heap]

main :: IO ()
main = print $ hsort (V.fromList [4,7,8,1,5,3,2,9,6]) == V.fromList [9..1]
