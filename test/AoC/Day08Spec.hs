module AoC.Day08Spec where

import AoC.Day08
import SpecHelper

spec :: Spec
spec =
  describe "Day 08" $ do
    it "solves part 1" $ do
      solution <- solvePart01 . fst <$> testInput
      solution `shouldBe` 6

    it "solves part 2" $ do
      solution <- solvePart02 . snd <$> testInput
      solution `shouldBe` 6
