module HammingNumbers where

import Data.List (sort, nub)
import Data.Function (fix)
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.All

hamming = 1 : map (2*) hamming `union` map (3*) hamming `union` map (5*) hamming

union a@(x:xs) b@(y:ys) = case compare x y of
  LT -> x : union xs b
  EQ -> x : union xs ys
  GT -> y : union a ys

-- point free version
hamming2 = fix ((1 :) . ap (union . ap (union . map (2 *)) (map (3 *))) (map (5 *)))


-- this one doesnt work yet
-- hamming3 = sort $ [1] ++ (sort . nub $ concatMap (\x -> [x*2, x*3, x*5]) hamming3)

main = do
  print $ take 20 hamming
  print  (hamming !! (1691-1), hamming !! (1692-1))
  print $ hamming !! (1000000-1)

main2 = do
  print $ take 20 hamming2
  print  (hamming2 !! (1691-1), hamming2 !! (1692-1))
  print $ hamming2 !! (1000000-1)


-- better version were it avoid regenerating duplicate values
hamming4 = 1 : foldr u [] [2,3,5] where
  u n s = r where
             r = merge s (map (n*) (1:r))

merge [] b = b
merge a@(x:xs) b@(y:ys) | x < y = x : merge xs b
                        | otherwise = y : merge a ys


prop_def_hamming_same_result n = take n hamming == take n hamming2
prop_def_hamming_million1 = hamming !! (1000000-1) == hamming2 !! (1000000-1)

prop_def_hamming_same_result2 n = take n hamming == take n hamming4
prop_def_hamming_million2 = hamming !! (1000000-1) == hamming4 !! (1000000-1)
