{-# LANGUAGE TemplateHaskell #-}

module ExploreArrays where

import Data.Array
import qualified Data.Map.Strict as M

squares :: Array Integer Integer
squares = array (1,100) [(i, i*i) | i <- [1..100]]

mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
mkArray f bnds = array bnds [(i, f i ) | i <- range bnds]

squares' :: Array Integer Integer
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
                  where ((_, jLo), (_, jHi)) = bounds a

swapRows2 :: (Ix a, Ix b, Enum b) => a -> a -> Array (a,b) c -> Array (a,b) c
swapRows2 i i' a = a // [assoc | j <- [jLo..jHi],
                                 assoc <- [((i,j), a!(i',j)),
                                           ((i',j), a!(i,j))]]
                   where ((_, jLo), (_,jHi)) = bounds a


hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
hist bnds is = accumArray (+) 0 bnds [(i,1) | i <- is , inRange bnds i]

testHist a1 = hist (0,10) (concat $ take 2 $ repeat [1..20])

testHist2 a1 = hist (0,10) (concat $ replicate 2 [1..20])


-- sorting a list by counting the nb of time each element occurs

count :: [Int] -> Int -> Array Int Int
count xs m = accumArray (+) 0 (0,m) (zip xs (repeat 1))

sort xs m = concat [replicate c x | (x,c) <- assocs (count xs m)]

-- Array counters
arrcount :: [Int] -> M.Map Int Int
arrcount xs = M.fromListWith (+) $ zip xs (repeat 1)
-- arrcount can be written with accumArray
arrcount2 :: [Int] -> Array Int Int
arrcount2 xs = accumArray (+) 0 bnds [(i,1) | i <- xs, inRange bnds i]
                 where bnds = (1, (maximum xs))

-- from FP pearls
-- checklist :: [Int] -> Array Int Bool
-- checklist xs = accumArray or
