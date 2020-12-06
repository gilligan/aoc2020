{-# LANGUAGE OverloadedStrings #-}

module AOC.Day3Spec (spec) where

import AOC.Day03
import Test.Hspec

spec :: Spec
spec = do
  describe "AOC" $ do
    describe "part1" $ do
      it "foo" $ do
        day3 `shouldBe` "day3"
