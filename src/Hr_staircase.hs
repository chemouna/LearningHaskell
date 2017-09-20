module Hr_staircase where

import Control.Applicative
import Control.Monad
import System.IO
import Test.Hspec


{-
   #
  ##
 ###
####
-}

solution :: Int -> [String]
solution 0 = []
solution n = map (\x -> take x (repeat '#')) [1..n] 

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    print n

spec :: Spec
spec =
  describe "Solution to Staircase problem" $ do
    it "Special Case for n = 0" $ do
      solution 0 `shouldBe` []
    it "Special case for
    n = 1" $ do
      solution 1 `shouldBe` ["#"]
    it "Case n = 2" $ do
      solution 2 `shouldBe` ["#", "##"]
    it "Case n = 4" $ do
      solution 4 `shouldBe` ["#", "##", "###", "####"]
