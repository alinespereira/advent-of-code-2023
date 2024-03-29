module AoC.Day02Spec where

import SpecHelper

import AoC.Day02

spec :: Spec
spec =
  describe "Day 02" $ do
    it "solves part 1" $ do
      solution <- solvePart01 <$> testInput
      solution `shouldBe` 8

    it "solves part 2" $ do
      solution <- solvePart02 <$> testInput
      solution `shouldBe` 2286