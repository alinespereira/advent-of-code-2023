module AoC.Day09 where

import AoC (readLines)

testInput :: IO [String]
testInput = readLines "data/Day09/test.txt"

dataInput :: IO [String]
dataInput = readLines "data/Day09/data.txt"

type History = [Int]

type Predictions = [Int]

parseLine :: String -> [Int]
parseLine = map read . words

diffSequence :: History -> History
diffSequence h = zipWith (-) (tail h) h

deriveHistory :: History -> [History]
deriveHistory h
  | all (== 0) h = []
  | otherwise = h : deriveHistory (diffSequence h)

predictSubseriesForward :: [History] -> Predictions
predictSubseriesForward = tail . foldr f [0]
  where
    f :: History -> Predictions -> Predictions
    f x acc = acc ++ [last x + last acc]

predictSubseriesBackward :: [History] -> Predictions
predictSubseriesBackward = init . foldr f [0]
  where
    f :: History -> Predictions -> Predictions
    f x acc = head x - head acc : acc

predictHistoryForward :: History -> Int
predictHistoryForward = last . predictSubseriesForward . deriveHistory

predictHistoryBackward :: History -> Int
predictHistoryBackward = head . predictSubseriesBackward . deriveHistory

solvePart01 :: [String] -> Int
solvePart01 = sum . map (predictHistoryForward . parseLine)

solvePart02 :: [String] -> Int
solvePart02 = sum . map (predictHistoryBackward . parseLine)
