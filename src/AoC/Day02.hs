module AoC.Day02 (testInput, 
                  dataInput,
                  solvePart01,
                  solvePart02) where

import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import AoC (readLines)

testInput :: IO [String]
testInput = readLines "data/Day02/test.txt"

dataInput :: IO [String]
dataInput = readLines "data/Day02/data.txt"

data Draw = Draw
  { getRed :: Maybe Int,
    getGreen :: Maybe Int,
    getBlue :: Maybe Int
  }
  deriving (Show)

data Game = Game {getId :: Int, getRuns :: [Draw]}
  deriving (Show)

strip :: (Char -> Bool) -> String -> String
strip fn s = dropWhileEnd fn $ dropWhile fn s

splitAtChar :: Char -> String -> [String]
splitAtChar _ "" = []
splitAtChar c s =
  let sub = takeWhile (/= c) s
      rest = strip (== c) $ dropWhile (/= c) s
   in sub : splitAtChar c rest

parseLine :: String -> Game
parseLine line =
  let game : sets : _ = map (strip isSpace) $ splitAtChar ':' line
      gameId :: Int
      gameId = read $ takeWhile isDigit $ dropWhile (not . isDigit) game
   in Game gameId $ parseSets sets

parseSets :: String -> [Draw]
parseSets sets =
  let draws = map (strip isSpace) $ splitAtChar ';' sets
   in map parseDraw draws

parseDraw :: String -> Draw
parseDraw draw =
  let sample = map (strip isSpace) $ splitAtChar ',' draw
      s = Map.fromList [(head c, read n :: Int) | n : c : _ <- map (splitAtChar ' ') sample]
   in Draw
        { getRed = Map.lookup 'r' s,
          getGreen = Map.lookup 'g' s,
          getBlue = Map.lookup 'b' s
        }

redMax :: Int
redMax = 12

greenMax :: Int
greenMax = 13

blueMax :: Int
blueMax = 14

solvePart01 :: [String] -> Int
solvePart01 = sum . map getId . filter validGame . map parseLine
  where
    validDraw :: Draw -> Bool
    validDraw (Draw r g b) =
      let red = maybe True (<= redMax) r
          green = maybe True (<= greenMax) g
          blue = maybe True (<= blueMax) b
       in red && green && blue
    validGame :: Game -> Bool
    validGame (Game _ draws) = all validDraw draws

solvePart02 :: [String] -> Int
solvePart02 = sum . map (gamePower . parseLine)
  where
    gamePower :: Game -> Int
    gamePower (Game _ draws) =
      let red = maximum $ map (fromMaybe 0 . getRed) draws
          green = maximum $ map (fromMaybe 0 . getGreen) draws
          blue = maximum $ map (fromMaybe 0 . getBlue) draws
       in red * green * blue
