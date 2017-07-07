module Exploring where


--pb 1 : Find the sum of all the even-valued terms in the Fibonacci sequence which
-- do not exceed one million.

prob1_sol1 :: Integer
prob1_sol1  = sum [x | x <- takeWhile (<= 1000000) fibs, even x]
  where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- factorial n = factorial (n - 1) * n

zipMap1 :: [a -> b] -> [a] -> [b]
zipMap1 [] _ = []
zipMap1 _ [] = []
zipMap1 (f:fs)(x:xs) = (f x):(zipMap1 fs xs)

zipMap2 :: [a -> b] -> [a] -> [b]
zipMap2 fs xs = map fstOnSnd (zip fs xs)
  where fstOnSnd (f,x) = f x

zipMap3 :: [a -> b] -> [a] -> [b]
zipMap3 = zipWith ($)

-- and applying combinators : S = zipMap and K = repeat we can write map, zip, zipWith, zipWith3
s = zipWith ($)
k = repeat

mapT f xs = s (k f) xs
zipWithT f as bs     = s (mapT f as) bs
zipT                 = zipWithT (,)
zipWith3T f as bs cs = s (zipWithT f as bs) cs

-- <$> -> is infix synonym of fmap

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM f as       =  sequence (map f as)

-- mapM f as = foldr k (return []) as
--             where
--               k a r = do { x <- f a; xs <- r; return (x:xs) }

-- sequence :: Monad m => [m a] -> m [a]
-- sequence ms = foldr k (return []) ms
--             where
--               k m m' = do { x <- m; xs <- m'; return (x:xs) }

-- 2nd way to implement it
-- sequence = mapM id

-- sequence implementation using foldr is like the impl of mapM using foldr but not
-- applying a fn which is equivalent to applying id -> sequence = mapM id

--  unionBy (on (==) fst)

-- execState ?? [] $ modify (1:)

-- instance Monoid a => Monoid (IO a) where
--     mempty = pure mempty
--     mappend = liftA2 mappend

-- fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- fromList [(5,'a'), (3,'b')] ! 5 == 'a'


-- all p = getAll #. foldMap (All #. p)

    -- fold :: Monoid m => t m -> m
    -- fold = foldMap id

    -- -- | Map each element of the structure to a monoid,
    -- -- and combine the results.
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    -- foldMap f = foldr (mappend . f) mempty

    -- elem :: Eq a => a -> t a -> Bool
    -- elem = any . (==)

-- any :: Foldable t => (a -> Bool) -> t a -> Bool
-- any p = getAny #. foldMap (Any #. p)
