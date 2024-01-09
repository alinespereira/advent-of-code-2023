module AoC.Day04
  ( testInput,
    dataInput,
    solvePart01,
    solvePart02,
  )
where

import AoC (readLines)
import Data.Char (isDigit)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import qualified Data.Map as M

testInput :: IO [String]
testInput = readLines "data/Day04/test.txt"

dataInput :: IO [String]
dataInput = readLines "data/Day04/data.txt"

data Card = Card
  { getId :: Int,
    getWinningNumbers :: [Int],
    getNumbers :: [Int]
  }
  deriving (Show)

instance Eq Card where
  (Card id1 _ _) == (Card id2 _ _) = id1 == id2

instance Ord Card where
  compare (Card id1 _ _) (Card id2 _ _) = compare id1 id2

parseCard :: String -> Card
parseCard line =
  let (cardIdStr, rest) = splitAtCharOnce ':' line
      (winningNumbersStr, numbersStr) = splitAtCharOnce '|' rest
      cardId = read $ dropWhile (not . isDigit) cardIdStr
      winningNumbers = map read $ words winningNumbersStr
      numbers = map read $ words numbersStr
   in Card cardId winningNumbers numbers

countMatches :: Card -> Int
countMatches (Card _ winningNumbers numbers) =
  length $ filter (`elem` winningNumbers) numbers

scoreCard :: Card -> Int
scoreCard card =
  let matches = countMatches card
      matchesAfterFirst = max 0 (matches - 1)
      afterFirstScores = map (2 ^) [0 ..]
      scores = 1 : take matchesAfterFirst afterFirstScores
   in sum $ take matches scores

wonScratchCards :: [Card] -> M.Map Card Int
wonScratchCards cards =
  let sortedCards = sort cards
      acc = M.fromList $ zip cards [1 ..]
    in wonScratchCards' sortedCards acc

wonScratchCards' :: [Card] -> M.Map Card Int -> M.Map Card Int
wonScratchCards' [] acc = acc
wonScratchCards' (c : cs) acc =
  let matches = countMatches c
      wonCards = take matches cs
      wons = read (fromJust $ M.lookup c acc) :: Int
      newAcc = updateWonScratchCards wonCards wons acc
   in wonScratchCards' cs acc


updateWonScratchCards :: [Card] -> Int -> M.Map Card Int -> M.Map Card Int
updateWonScratchCards [] _ acc = acc
updateWonScratchCards (c : cs) n acc =
  let newAcc = M.inserWith (+) c n acc
    in updateWonScratchCards cs n newAcc
  

splitAtCharOnce :: Char -> String -> (String, String)
splitAtCharOnce c s =
  case elemIndex c s of
    Nothing -> (s, "")
    Just i ->
      let (s', _ : rest) = splitAt i s
       in (s', rest)

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

solvePart01 :: [String] -> Int
solvePart01 = sum . map (scoreCard . parseCard)

solvePart02 :: [String] -> Int
solvePart02 input =
  let cards = map parseCard input
      won = wonScratchCards cards
   in sum $ M.elems won
