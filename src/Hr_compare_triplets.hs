{-# LANGUAGE OverloadedStrings #-}
module Hr_compare_triplets where

import Control.Applicative
import Control.Monad
import System.IO
import Data.List
import Data.String as S

foo :: (Int, Int) -> ([Int], [Int])
foo (x, y) = if x < y then ([1], []) else if x > y then ([], [1]) else ([], [])

cmpPair :: Int -> Int -> ([Int], [Int])
cmpPair x y = if x < y then ([], [1]) else if x > y then ([1], []) else ([], [])

mapTriplets :: (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int)]
mapTriplets (a0, a1, a2) (b0, b1, b2) = [(a0, b0), (a1, b1), (a2, b2)]

-- [([1],[]),([1],[]),([1],[])] want to go from it to [(3,0)]


printElements :: [Int] -> IO()
printElements [] = return ()
printElements [x] = do putStr $ show x
printElements (x:xs) = do putStr $ show x
                          putStr " "
                          printElements xs

-- this solution is not woking 100% yet
main :: IO ()
main = do
    a0_temp <- getLine
    let a0_t = words a0_temp
    let a0 = read $ a0_t !! 0 :: Int
    let a1 = read $ a0_t !! 1 :: Int
    let a2 = read $ a0_t !! 2 :: Int
    b0_temp <- getLine
    let b0_t = words b0_temp
    let b0 = read $ b0_t !! 0 :: Int
    let b1 = read $ b0_t !! 1 :: Int
    let b2 = read $ b0_t !! 2 :: Int
    printElements $ concat (map (uncurry (++)) (map (uncurry cmpPair) ( mapTriplets (a0, a1, a2) (b0, b1, b2))))

main2 :: IO ()
main2 = do
    a0_temp <- getLine
    let a0_t = words a0_temp
    let a0 = read $ a0_t !! 0 :: Int
    let a1 = read $ a0_t !! 1 :: Int
    let a2 = read $ a0_t !! 2 :: Int
    b0_temp <- getLine
    let b0_t = words b0_temp
    let b0 = read $ b0_t !! 0 :: Int
    let b1 = read $ b0_t !! 1 :: Int
    let b2 = read $ b0_t !! 2 :: Int
    let (alice, bob) = getAliceBob [a0, a1, a2] [b0, b1, b2]
    putStrLn $ show alice ++ " " ++ show bob

getAliceBob :: [Int] -> [Int] -> (Int, Int)
getAliceBob [] [] = (0,0)
getAliceBob (a:as) (b:bs) = let (alice, bob) = getAliceBob as bs
                                in
                                  if a > b then (alice + 1, bob)
                                  else if a < b then (alice, bob + 1)
                                  else (alice, bob)


