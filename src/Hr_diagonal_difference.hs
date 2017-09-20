module Hr_Diagonal_difference where

solution list n = abs(sum_diag1 - sum_diag2)
  where
    sum_diag1 = sum (zipWith (\ x y -> x !! y) list [0..n-1])
    sum_diag2 = sum (zipWith (\ x y -> x !! y) list (enumFromThenTo (n-1) 1 0))


getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getMultipleLines n
    let a = map ( map ( read :: String -> Int ) . words ) a_temp
    print $ solution a n
