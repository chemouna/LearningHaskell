{-# LANGUAGE OverloadedStrings #-}

module Whisper where

import Data.Strings
import Data.List.Split
import Data.List
import Data.Ord
import Test.Hspec
import Data.Char

-- String toWhom(String[] usernames, String typed)

toWhom :: [String] -> String -> String
toWhom us m =
  if strStartsWith ml "/msg " then findWhom us (diffLowerCase m msg)
  else  "not a whisper"
  where
    msg = "/msg "
    ml = strToLower m

findWhom :: [String] -> String -> String
findWhom us m
  | null candidates = "user is not logged in"
  | otherwise = maximumBy (comparing length) candidates
  where
    candidates = intersectBy (\x y -> strToLower x == strToLower y) ll us
    s = map init $ filter (\x -> strEndsWith x " ") $ splitOnKeepR " " m
    r = tail $ subsequences s
    z = filter (\(x:xs) -> x == head s) r
    ll = map (concat . intersperse " ") z

splitOnKeepR delim s = split (keepDelimsR $ oneOf delim) s

diffLowerCase :: [Char] -> [Char] -> [Char]
diffLowerCase =  foldl (flip deleteByLower)

deleteByLower             ::  Char -> [Char] -> [Char]
deleteByLower _ []     = []
deleteByLower x (y:ys) = if x' == y' then ys else y : deleteByLower x ys
  where
    x' = toLower x
    y' = toLower y


main :: IO ()
main = hspec $ do
  describe "toWhom" $ do
    it "Case 1" $ do
      toWhom ["John","John Doe","John Doe h"] "/msg John Doe hi there" `shouldBe` "John Doe"

    it "Case 2" $ do
      toWhom ["John","John Doe","John Doe h"] "/MSG jOHN dOE HI THERE" `shouldBe` "John Doe"

    it "Case 3" $ do
      toWhom ["writer"] "writer hi" `shouldBe` "not a whisper"

    it "Case 4" $ do
      toWhom ["tester"] "/msg testerTwo you there" `shouldBe` "user is not logged in"

    it "Case 5" $ do
      toWhom ["lbackstrom"] "/msg lbackstrom" `shouldBe` "user is not logged in"

    it "Case 6" $ do
      toWhom ["me"] "/msg me  hi" `shouldBe` "user is not logged in"

    it "Case 7" $ do
      toWhom ["abc"] " /msg abc note the leading space" `shouldBe` "not a whisper"

    it "Case 8" $ do
      toWhom ["Wow"]"/msg Wow " `shouldBe` "Wow"

    it "Case 9" $ do
      toWhom ["msg"] "/msg" `shouldBe`  "not a whisper"
