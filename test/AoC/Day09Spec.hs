module AoC.Day09Spec where

import AoC.Day09
import SpecHelper

spec :: Spec
spec =
  describe "Day 09" $ do
    it "solves part 1" $ do
      solution <- solvePart01 <$> testInput
      solution `shouldBe` 114

    it "solves part 2" $ do
      solution <- solvePart02 <$> testInput
      solution `shouldBe` 2
