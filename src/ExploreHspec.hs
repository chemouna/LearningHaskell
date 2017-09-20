module ExploreHspec where

import Test.Hspec


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs


spec :: Spec
spec =
  describe "Example safeHead" $ do
    it "returns the head" $ do
      safeHead [1,2,3] `shouldBe` Just 1

    it "return Nothing for empty list" $ do
      safeHead [] `shouldBe` (Nothing :: Maybe Int)
