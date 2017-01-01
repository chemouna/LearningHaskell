
module ConcatMapUsage where


-- | Problem 15 :  Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- pointFree version
repli_pf xs n = replicate n =<< xs
repli_pf' = (. replicate) . (>>=)
