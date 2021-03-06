module Hr_Diagonal_difference where


solution list n = abs(sum_diag1 - sum_diag2)
  where
    sum_diag1 = sum (zipWith (\ x y -> x !! y) list [0..n-1])
    sum_diag2 = sum (zipWith (\ x y -> x !! y) list (enumFromThenTo (n-1) 1 0))

solution2 list n = abs (p1 - p2)
  where
    (p1, p2) = p
    p = foldl (\ acc@(x', y') (x,y) -> (x' + x, y' + y)) (0,0) pairs
    pairs = zipWith3 (\ x y z -> (x !! y, x !! z)) list [0..n-1]  (enumFromThenTo (n-1) 1 0)


solution3 :: [[Int]] -> Int
solution3 matrix = abs $ foldl rowDiff 0 indexedRows
  where rowDiff acc (i, row) = acc + (row !! i) - (row !! (n-1-i))
        n = length matrix
        indexedRows = zip [0..] matrix

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
    print a
    print $ solution3 a
