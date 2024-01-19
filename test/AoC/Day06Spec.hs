module AoC.Day06Spec where

import SpecHelper

import AoC.Day06

spec :: Spec
spec =
  describe "Day 06" $ do
    it "solves part 1" $ do
      solution <- solvePart01 <$> testInput
      solution `shouldBe` 288

    it "solves part 2" $ do
      solution <- solvePart02 <$> testInput
      solution `shouldBe` 71503
