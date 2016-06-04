
module Lists where

-- Unzipping

myUnzip :: [(a,b)] -> ([a], [b])
myUnzip [] = ([],[])
myUnzip ((x,y):tl) =
  let (xs,ys) = myUnzip tl
      in (x:xs,y:ys)

