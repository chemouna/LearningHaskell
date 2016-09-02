module LazyEvaluation where


repeat' a = x where x = a : x

-- TODO: Perfect numbers, hamming, sieves, ..

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

