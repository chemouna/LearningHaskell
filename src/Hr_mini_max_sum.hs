{-# LANGUAGE FlexibleContexts #-}
module Hr_mini_max_sum where

import Data.List (splitAt)

deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs

solution :: [Int] -> (Int, Int)
solution xs = (minimum sums, maximum sums)
  where
    sums = uncurry (\x y -> x) result
    result = foldr consSum ([], 0) xs
    consSum :: Int -> ([Int], Int) -> ([Int], Int)
    consSum x (acc, i) = ((sum (deleteAt i xs)) : acc, i + 1)

main :: IO ()
main = do
    arr_temp <- getLine
    let arr = map read $ words arr_temp :: [Int]
    putStr ( unwords (map show (uncurry (\x y -> [x, y]) (solution arr))))

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret
