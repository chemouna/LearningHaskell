module Hr_staircase where

import Control.Applicative
import Control.Monad
import System.IO
import Test.Hspec
import Data.List

{-
   #
  ##
 ###
####
-}

pad :: Char -> [String] -> [String]
pad c xs = ys where
  (ys, n)         = foldr cons ([],0) xs
  cons x (acc, m) = ((replicate (n - m') c ++ x) : acc, max m m')
    where m' = length x

solution :: Int -> [String]
solution 0 = []
solution n = pad ' ' (map (\x -> take x (repeat '#')) [1..n])

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    mapM_ putStrLn (solution n)

-- Solution 2
solution2 :: Int -> [String]
solution2 n = map (\i -> replicate (n - i) ' ' ++ replicate i '#') [1..n]

main2 :: IO ()
main2 = readLn >>= mapM_ putStrLn . solution2

--

spec :: Spec
spec =
  describe "Solution to Staircase problem" $ do
    it "Special Case for n = 0" $ do
      solution 0 `shouldBe` []
    it "Special case for n = 1" $ do
      solution 1 `shouldBe` ["#"]
    it "Case n = 2" $ do
      solution 2 `shouldBe` [" #", "##"]
    it "Case n = 4" $ do
      solution 4 `shouldBe` ["   #", "  ##", " ###", "####"]
