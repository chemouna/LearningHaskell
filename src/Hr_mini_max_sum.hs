{-# LANGUAGE FlexibleContexts #-}
module Hr_mini_max_sum where

import Data.List (splitAt, sort)

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

-- Solution 2
main2 = fmap (map (\x -> read x :: Int).words) getLine >>= (putStr.unwords.(map show).(\l -> [sum l - maximum l, sum l - minimum l]))

-- Solution 3
main3 :: IO ()
main3 = do
    arr_temp <- getLine
    let arr = map read $ words arr_temp :: [Integer]
    let numCombos = combos 4 arr
    let sortedSums = sort $ map sum numCombos
    let minN = show $ head sortedSums
    let maxN = show $ head $ reverse sortedSums
    putStrLn $ minN ++ " " ++ maxN


combos :: Int -> [a] -> [[a]]
combos n xs =
  if length xs == n
  then [xs]
  else case xs of
        []    -> []
        (x:t) -> sprinkle x (combos (n-1) t) ++ combos n t

sprinkle :: a -> [[a]] -> [[a]]
sprinkle x ys =
  case ys of
    []    -> []
    (y:t) -> (x:y) : sprinkle x t

--

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret
