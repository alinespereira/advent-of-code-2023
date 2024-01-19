module AoC.Day06 where

import AoC (readLines)

testInput :: IO [String]
testInput = readLines "data/Day06/test.txt"

dataInput :: IO [String]
dataInput = readLines "data/Day06/data.txt"

data Race = Race
  { time :: Int
  , distance :: Int }
  deriving (Show, Eq)

parseInput :: [String] -> [Race]
parseInput input = zipWith Race ts ds
  where
    ts = parseRowOfNumbers (head input)
    ds = parseRowOfNumbers (input !! 1)

parseRowOfNumbers :: String -> [Int]
parseRowOfNumbers = tail . map read . words

parseSpacedNumber :: String -> Int
parseSpacedNumber = read . concat . tail . words

parseInput1 :: [String] -> Race
parseInput1 input = Race t d
  where
    t = parseSpacedNumber $ head input
    d = parseSpacedNumber $ input !! 1

race :: Race -> Int -> (Bool, Int)
race (Race t d) hold
  | hold >= t = (False, 0)
  | otherwise = (dist > d, dist)
    where
      speed = hold
      remTime = t - hold
      dist = speed * remTime

recordRaces :: Race -> [(Bool, Int)]
recordRaces r@(Race t _) = map (race r) [0 .. t] 

wins :: Race -> Int
wins = length . filter fst . recordRaces

solvePart01 :: [String] -> Int
solvePart01 = product . map wins . parseInput

solvePart02 :: [String] -> Int
solvePart02 = wins . parseInput1