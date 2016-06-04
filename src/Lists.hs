{-# LANGUAGE TemplateHaskell #-}

module Lists where

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


