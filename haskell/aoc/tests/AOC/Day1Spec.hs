module AOC.Day1Spec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "AOC" $ do
      describe "nothing" $ do
          it "is always true" $ do
              True `shouldBe`True
