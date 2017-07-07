
module TraversableUsage where

half x = if even x then Just (x `div` 2) else Nothing

-- traverse half [2,4..10] -- returns Just [1,2,3,4,5]
-- traverse half [1..10] -- returns Nothing -- because Traversable uses <*> to combine the results

