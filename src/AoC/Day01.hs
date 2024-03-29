module AoC.Day01 (testInput, 
                  dataInput,
                  solvePart01,
                  solvePart02) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

import AoC (readLines)

testInput :: IO ([String], [String])
testInput = do
  test01 <- readLines "data/Day01/test01.txt"
  test02 <- readLines "data/Day01/test02.txt"
  return (test01, test02)

dataInput :: IO [String]
dataInput = readLines "data/Day01/data.txt"

findDigits :: String -> [Char]
findDigits = filter isDigit

findAllDigits :: String -> [Char]
findAllDigits [] = []
findAllDigits input@(x : xs)
  | "one" `isPrefixOf` input = '1' : findAllDigits xs
  | "two" `isPrefixOf` input = '2' : findAllDigits xs
  | "three" `isPrefixOf` input = '3' : findAllDigits xs
  | "four" `isPrefixOf` input = '4' : findAllDigits xs
  | "five" `isPrefixOf` input = '5' : findAllDigits xs
  | "six" `isPrefixOf` input = '6' : findAllDigits xs
  | "seven" `isPrefixOf` input = '7' : findAllDigits xs
  | "eight" `isPrefixOf` input = '8' : findAllDigits xs
  | "nine" `isPrefixOf` input = '9' : findAllDigits xs
  | isDigit x = x : findAllDigits xs
  | otherwise = findAllDigits xs

findCalibrationValue :: [Char] -> Int
findCalibrationValue line =
  let digits = findDigits line
      firstDigit = head digits
      lastDigit = last digits
   in (digitToInt firstDigit * 10) + digitToInt lastDigit

solvePart01 :: [String] -> Int
solvePart01 = sum . map (findCalibrationValue . findDigits)

solvePart02 :: [String] -> Int
solvePart02 = sum . map (findCalibrationValue . findAllDigits)
