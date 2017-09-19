module Hr_compare_triplets where

import Control.Applicative
import Control.Monad
import System.IO

-- res ([Int], [Int]) = (AlicePoints, BobPoints)
-- compareTriplets :: (Int, Int, Int) -> (Int, Int, Int) -> ([Int], [Int])
-- compareTriplets  (a0, a1, a2) (b0, b1, b2)
--  | a0 > b0  = ([1], [0])
--  | a2

-- lets first try for a simpler problem with only a pair
comparePair :: (Int, Int) -> (Int, Int) -> ([Int], [Int])
comparePair v1@(a0, a1) v2@(b0, b1) = foldl1 (\ (x1, x2) (y1, y2) -> val) ([], [])
  where val
          | a0 < b0 || a1 < b1 = ([1], [])
          | a0 > b0 || a1 > b1 = ([], [1])

-- try to use zip
-- try to write in terms of list comprehension in the same way as the equation is written


main :: IO ()
main = do
    a0_temp <- getLine
    let a0_t = words a0_temp
    let a0 = read $ a0_t !! 0 :: Int
    let a1 = read $ a0_t !! 1 :: Int
    let a2 = read $ a0_t !! 2 :: Int
    b0_temp <- getLine
    let b0_t = words b0_temp
    let b0 = read $ b0_t !! 0 :: Int
    let b1 = read $ b0_t !! 1 :: Int
    let b2 = read $ b0_t !! 2 :: Int
    print $ "Test"
--    compareTriplets (a0, a1, a2) (b0, b1, b2)
