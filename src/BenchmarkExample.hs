
module BenchmarkExample where

import Data.List
import qualified Data.Set as Set
import Criterion.Main 
import System.Random
import Control.Applicative

main :: IO ()
main = defaultMain [
    bgroup "input1" [
      bench "nub" $ nfIO $ nub <$> generateInput (0, 100) 1000,
      bench "poorsod1" $ nfIO $ poorsod1 <$> generateInput (0, 100) 1000,
      bench "poorsod2" $ nfIO $ poorsod2 <$> generateInput (0, 100) 1000,
      bench "volkov" $ nfIO $ volkov <$> generateInput (0, 100) 1000,
      bench "scvalex" $ nfIO $ scvalex <$> generateInput (0, 100) 1000
    ],
    bgroup "input2" [
      bench "nub" $ nfIO $ nub <$> generateInput (0, 1000) 1000,
      bench "poorsod1" $ nfIO $ poorsod1 <$> generateInput (0, 1000) 1000,
      bench "poorsod2" $ nfIO $ poorsod2 <$> generateInput (0, 1000) 1000,
      bench "volkov" $ nfIO $ volkov <$> generateInput (0, 1000) 1000,
      bench "scvalex" $ nfIO $ scvalex <$> generateInput (0, 1000) 1000
    ]
  ]



generateInput :: (Int, Int) -> Int -> IO [Int]
generateInput range amount = 
  sequence $ replicate amount $ randomRIO range

sol1 :: Eq a => [a] -> [a]
sol1 = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

sol2 :: Eq a => [a] -> [a]
sol2 = foldl (\seen x -> if x `elem` seen
                              then seen
                              else seen ++ [x]) []

sol3 :: Ord a => [a] -> [a]
sol3 = rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if Set.member b a
    then rmdups' a c
    else b : rmdups' (Set.insert b a) c

sol4 :: (Ord a) => [a] -> [a]
sol4 = map head . group . sort


