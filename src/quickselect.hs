module QuickSelect where

import qualified Data.List as L
import qualified Data.Maybe as M

import Test.QuickCheck

groupIn :: Int -> [a] -> [[a]]
groupIn _ [] = []
groupIn n xs = h : groupIn n t
  where (h, t) = splitAt n xs

medianOfMedians :: (Ord a, Show a) => [a] -> a
medianOfMedians xs
  | len == 0 = error "cannot find median of empty list"
  | len < 6 = median xs
  | otherwise = (medianOfMedians . map median . groupIn 5) xs
  where len = length xs
        median ls = L.sort ls !! (length ls `div` 2)

noNothing :: Int
noNothing = -1

outOfBounds :: Int -> Int -> Bool
outOfBounds i len = i < 0 || i > (len - 1)

partition :: (Ord a, Show a) => a -> [a] -> [a]
partition x xs =
    filter (< x) xs ++ filter (== x) xs ++ filter (> x) xs

select :: (Ord a, Show a) => Int -> [a] -> a
select 0 [x] = x
select n xs
  | outOfBounds n len = error "n out of bounds in select"
  | n == pivotInd     = pxs !! n
  | n < pivotInd      = select n (slice 0 (pivotInd - 1) pxs) -- left, right sides
  | otherwise         = select (n - pivotInd - 1) (slice (pivotInd + 1) (len - 1) pxs)
    where len         = length xs
          pivot    = medianOfMedians xs
          pivotInd = M.fromMaybe noNothing $ L.elemIndex pivot pxs
          pxs      = partition pivot xs
          slice from to = take (to - from + 1) . drop from



prop_select :: Int -> NonEmptyList Int -> Bool
prop_select position (NonEmpty l) = let position' = position `mod` length l in
                        (L.sort l !! position') == select position' l
