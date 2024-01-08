module AoC.Day03 (testInput, 
                  dataInput,
                  solvePart01,
                  solvePart02) where

import Data.Char (isDigit)
import Data.List (partition)
import Data.Maybe (mapMaybe)

import AoC (readLines)

testInput :: IO [String]
testInput = readLines "data/Day03/test.txt"

dataInput :: IO [String]
dataInput = readLines "data/Day03/data.txt"

data Position = Position
  { getRow :: Int,
    getCol :: Int
  }
  deriving (Show, Eq)

data Offset = Offset
  { getDy :: Int,
    getDx :: Int
  }
  deriving (Show, Eq)

walk :: Position -> Offset -> Position
walk (Position row col) (Offset dy dx) =
  Position (row + dy) (col + dx)

eightNeighbours :: [Offset]
eightNeighbours =
  filter
    (/= Offset 0 0)
    [Offset dy dx | dy <- [-1, 0, 1], dx <- [-1, 0, 1]]

data Number = Number
  { getNumber :: Int,
    getSpan :: [Position]
  }
  deriving (Show)

data Symbol = Symbol
  { getSymbol :: Char,
    getPos :: Position
  }
  deriving (Show)

mkNumber :: Int -> [(Char, Int)] -> Number
mkNumber row numPos =
  let number = read $ map fst numPos
      pos = map snd numPos
      numSpan = map (Position row) pos
   in Number number numSpan

parseLineNumbers :: Int -> String -> [Number]
parseLineNumbers _ [] = []
parseLineNumbers idx line =
  let pairs = zip line [0 ..]
      (digits, _) = partition (isDigit . fst) pairs
      splits = splitPoints (map snd digits)
      nums = splitAtMany splits digits
   in map (mkNumber idx) nums

parseLineSymbols :: Int -> String -> [Symbol]
parseLineSymbols _ [] = []
parseLineSymbols idx line =
  let pairs = zip line [0 ..]
      isSymbol :: Char -> Bool
      isSymbol c = c /= '.' && not (isDigit c)
      (symbols, _) = partition (isSymbol . fst) pairs
   in map (\(s, c) -> Symbol s $ Position idx c) symbols

isPartNumber :: [Offset] -> [Symbol] -> Number -> Bool
isPartNumber offsets symbols (Number _ pos) =
  let neighbours = filter (`notElem` pos) $ concatMap (findNeighbours offsets) pos
   in any ((`elem` neighbours) . getPos) symbols

findGears :: [Offset] -> [Number] -> Symbol -> Maybe (Number, Number)
findGears offsets numbers (Symbol _ pos) =
  let neighbours = findNeighbours offsets pos
      isGearPart :: [Position] -> Number -> Bool
      isGearPart neighbours (Number _ pos) =
        any (`elem` neighbours) pos
      gearParts = filter (isGearPart neighbours) numbers
      isGearParts = length gearParts == 2
   in if isGearParts then Just (head gearParts, last gearParts) else Nothing

findNeighbours :: [Offset] -> Position -> [Position]
findNeighbours offsets pos = map (walk pos) offsets

splitPoints :: [Int] -> [Int]
splitPoints currentPos =
  let nextPos = tail currentPos
      posIndices = zip [1 ..] $ zip currentPos nextPos
      consecutive pos =
        let curr = fst $ snd pos
            next = snd $ snd pos
         in next - curr == 1
      midPoints = map fst (filter (not . consecutive) posIndices)
      lastIndex = last currentPos + 1
   in 0 : midPoints ++ [lastIndex]

splitAtMany :: [Int] -> [a] -> [[a]]
splitAtMany ps xs = splitAtManyLength takeLen xs
  where
    takeLen = zipWith (-) (tail ps) ps
    splitAtManyLength :: [Int] -> [a] -> [[a]]
    splitAtManyLength _ [] = []
    splitAtManyLength [] xs = [xs]
    splitAtManyLength (p : ps) xs =
      let (first, rest) = splitAt p xs
       in first : splitAtManyLength ps rest

solvePart01 :: [String] -> Int
solvePart01 input =
  let numbers = concat $ zipWith parseLineNumbers [0 ..] input
      symbols = concat $ zipWith parseLineSymbols [0 ..] input
   in sum $ map getNumber $ filter (isPartNumber eightNeighbours symbols) numbers

solvePart02 :: [String] -> Int
solvePart02 input =
  let numbers = concat $ zipWith parseLineNumbers [0 ..] input
      symbols = filter (\s -> getSymbol s == '*') $ concat $ zipWith parseLineSymbols [0 ..] input
      gears = mapMaybe (findGears eightNeighbours numbers) symbols
      gearFstNumbers = map (getNumber . fst) gears
      gearSndNumbers = map (getNumber . snd) gears
   in sum $ zipWith (*) gearFstNumbers gearSndNumbers
