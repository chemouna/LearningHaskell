{-# LANGUAGE TemplateHaskell #-}

module Lists where

import Data.Function (on)
import Data.List (sortBy, groupBy, delete, map)
import Data.Ord (comparing)
import Data.Map (fromListWith)

-- Unzipping

myUnzip :: [(a,b)] -> ([a], [b])
myUnzip [] = ([],[])
myUnzip ((x,y):tl) =
  let (xs,ys) = myUnzip tl
      in (x:xs,y:ys)

-- unzip with continuation passing style
unzipk :: [(a,b)] -> ([a] -> [b] -> d) -> d
unzipk [] k = k [] []
unzipk ((x,y):tl) k = 
 unzipk tl (\ xs ys -> k (x:xs) (y:ys))


-- grouping
dic = [(1,"aa"),(1,"cc"),(2,"aa"),(3,"ff"),(3,"gg"),(1,"bb")]

-- we want to group them to get : grp  = [(1,["aa","bb","cc"]), (2, ["aa"]), (3, ["ff","gg"])]
myGroup :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
myGroup = map (\x -> (fst . head $ x, map snd x)) . groupBy ((==) `on` fst) . sortBy (comparing fst)

myGroup2 xs = fromListWith (++) [(k, [v]) | (k, v) <- xs]


-- find all permutations of a list
permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
-- permute xs = concatMap (\x -> map (x:) $ permute $ delete x xs) xs

permute xs = concatMap (\x -> map (x:) (permute (delete x xs))) xs

