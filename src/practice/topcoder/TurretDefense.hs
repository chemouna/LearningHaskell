
module TurretDefense where

import Data.List
import Data.Maybe

turretDefense :: [Int] -> [Int] -> [Int] -> Int
turretDefense xs ys ts = fromMaybe (-1) (findIndex (\(x, y) -> x > y) $ zip tn ts)
  where
    tn = 0:zipWith (+) timeNeed (init ts)
    timeNeed = zipWith (+) diffX diffY
    diffX = diff xs
    diffY = diff ys
    diff l = map abs $ zipWith (-) (tail l) l

-- turretDefense [3,5,6] [7,5,6][11,15,16]

-- turretDefense [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] [2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32]


-- turretDefense [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] [2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,31]

