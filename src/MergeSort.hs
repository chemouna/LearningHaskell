
module MergeSort where


-- Laziness friendly merge sort
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort xs = foldtree1 merge $ map return xs

foldtree1 f [x] = x
foldtree1 f xs = foldtree1 f $ pairs xs
  where
    pairs [] = []
    pairs [x] = [x]
    pairs (x:x':xs) = f x x' : pairs xs

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then x:merge xs (y:ys) else y:merge (x:xs) ys 

