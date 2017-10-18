
module TurretDefense where


import Data.List
import Data.Maybe

-- foldl (0, 0, 0) (\(x,y,t0) (x',y',t1) -> (x, y, t0 + (abs(x'-x) + abs(y'-y)))) (zip3 xs ys t)

-- map abs $ zipWith (-) (tail ys) ys

-- map abs $ zipWith (-) (tail xs) xs

-- zipWith (+) timeNeed (init ts)

-- let timeNeed = zipWith (+) diffX diffY

-- let diffX = map abs $ zipWith (-) (tail xs) xs

-- findIndex (\(x, y) -> x > y) $ zip tn ts

--  let tn = 0:zipWith (+) timeNeed (init ts)

turretDefense :: [Int] -> [Int] -> [Int] -> Int
turretDefense xs ys ts = fromMaybe (-1) (findIndex (\(x, y) -> x > y) $ zip tn ts)
  where
    tn = 0:zipWith (+) timeNeed (init ts)
    timeNeed = zipWith (+) diffX diffY
    diffX = diff xs
    diffY = diff ys
    diff l = map abs $ zipWith (-) (tail l) l

-- turretDefense [3,5,6] [7,5,6][11,15,16]

  
