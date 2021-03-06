module Experiments where
import Data.List
import Data.Char

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

{- largest number under 100,000 that's divisible by 3829 -}
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

numLongChainsWithLambda :: Int
numLongChainsWithLambda = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum2' :: (Num a) => [a] -> a
sum2' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

mapWithFoldl' :: (a -> b) -> [a] -> [b]
mapWithFoldl' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse2' :: [a] -> [a]
reverse2' = foldl (flip (:)) []

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

isInAny' :: Eq a => [a] -> [[a]] -> Bool
isInAny' needle haystack = any (isInfixOf needle) haystack

mem, mem' :: Eq a => a -> [a] -> Bool
mem x lst = any (== x) lst
mem'      = any . (==)

dot :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
dot = ((.).(.))

-- mergesort    :: (Ord a) => [a] -> [a]
-- mergesort xs = foldt merge [] [[x] | x <- xs]

-- sequence [succ, pred, (*3)] 4

-- convert lower case letters into upper case
uppercase :: String -> String
uppercase = map (\c -> if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c)

uppercase2 :: String -> String
uppercase2 = map toUpper






