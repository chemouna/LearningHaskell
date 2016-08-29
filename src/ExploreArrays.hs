module ExploreArrays where

import Data.Array

squares = array (1,100) [(i, i*i) | i <- [1..100]]

mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
mkArray f bnds = array bnds [(i, f i ) | i <- range bnds]

squares' = mkArray (\i -> i*i) (1,100)

fibs :: Int -> Array Int Int
fibs n = a where a = array (0,n) ([(0,1), (1,1)] ++
                                [(i, a!(i-2) + a!(i-1)) | i <- [2..n]])

test :: Array Int Int
test = a where a = array (1,100) ([(1,1)] ++ [(i, i * a!(i-1)) | i <- [2..100]])

-- interchange two rows in a matrix
swapRows :: (Ix a, Ix b, Enum b) => a -> a -> Array (a,b) c -> Array (a,b) c
swapRows i i' a = a // ([((i, j), a!(i', j)) | j <- [jLo..jHi]] ++
                        [((i', j), a!(i,j)) | j <- [jLo..jHi]])
                  where ((iLo, jLo), (iHi, jHi)) = bounds a

swapRows2 :: (Ix a, Ix b, Enum b) => a -> a -> Array (a,b) c -> Array (a,b) c
swapRows2 i i' a = a // [assoc | j <- [jLo..jHi],
                                 assoc <- [((i,j), a!(i',j)),
                                           ((i',j), a!(i,j))]]
                   where ((iLo, jLo), (iHi,jHi)) = bounds a
