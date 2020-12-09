{-# LANGUAGE OverloadedStrings #-}

module AOC.Day3Spec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "AOC" $ do
    describe "part1" $ do
      it "foo" $ do
        ("i am lazy" :: String) `shouldBe` ("i am lazy" :: String)
