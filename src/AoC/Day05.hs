module AoC.Day05 where

import AoC (readLines)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Range
import Text.Parsec (parse)
import Text.Parsec.Char (digit, letter, spaces, string)
import Text.Parsec.Combinator (many1, sepBy)
import Text.Parsec.String (Parser)
import qualified Text.Printf as Printf

testInput :: IO [String]
testInput = readLines "data/Day05/test.txt"

dataInput :: IO [String]
dataInput = readLines "data/Day05/data.txt"

data Describer = Describer
  { getDestinationRange :: Range Int,
    getSourceRange :: Range Int
  }
  deriving (Eq)

instance Show Describer where
  show (Describer d s) =
    Printf.printf "Map { to: %s, from: %s }" (show d) (show s)

type Seeds = [Int]

type Almanac = M.Map (String, String) [Describer]

parseAlmanac :: [String] -> (Seeds, Almanac)
parseAlmanac input =
  let s : maps = input
   in (parseLine seeds s, parseMaps maps)

parseMaps :: [String] -> Almanac
parseMaps [] = M.empty
parseMaps input =
  let label : rest = dropWhile (== "") input
      ranges = takeWhile (/= "") rest

      fromTo = parseLine describerFromTo label
      describers = map (parseLine describerRanges) ranges
      m = M.fromList [(fromTo, describers)]

      input' = dropWhile (/= "") rest
   in M.union m (parseMaps input')

parseLine :: Parser a -> String -> a
parseLine p s =
  case parse p "" s of
    Left _ -> error "Parse error"
    Right res -> res

seeds :: Parser Seeds
seeds = string "seeds:" >> spaces >> manyNumbers

describerFromTo :: Parser (String, String)
describerFromTo = do
  from <- word
  _ <- string "-to-"
  to <- word
  spaces
  _ <- string "map:"
  return (from, to)

describerRanges :: Parser Describer
describerRanges = do
  ds : ss : l : _ <- manyNumbers
  let rd = ds +=* (ds + l)
  let rs = ss +=* (ss + l)
  return $ Describer rd rs

number :: Parser Int
number = read <$> many1 digit

word :: Parser String
word = many1 letter

manyNumbers :: Parser [Int]
manyNumbers = number `sepBy` spaces

getDestination :: Describer -> Int -> Maybe Int
getDestination (Describer rd rs) n
  | inRange rs n = 
    let ms = head $ fromRanges [rs]
        md = head $ fromRanges [rd]
      in Just (n - ms + md)
  | otherwise = Nothing

forward :: Almanac -> String -> String -> Int -> Int
forward almanac from to = go almanac path
  where
      path = getPath almanac from to
      go :: Almanac -> [(String, String)] -> Int -> Int
      go _ [] s = s
      go a (p : ps) s =
        let s' = forward' a p s
        in go a ps s'

forward' :: Almanac -> (String, String) -> Int -> Int
forward' almanac dmap seed =
  let describers = fromMaybe [] $ M.lookup dmap almanac
    in case mapMaybe (`getDestination` seed) describers of
      [] -> seed
      d : _ -> d

getPath :: Almanac -> String -> String -> [(String, String)]
getPath almanac from to =
  let (_, to') = getKeyFrom almanac from
    in if to' == to
      then [(from, to')]
      else (from, to') : getPath almanac to' to

getKeyFrom :: Almanac -> String -> (String, String)
getKeyFrom almanac from
  = head $ filter (\(f, _) -> f == from) $ M.keys almanac

getKeyTo :: Almanac -> String -> (String, String)
getKeyTo almanac to
  = head $ filter (\(_, t) -> t == to) $ M.keys almanac

forwardRange :: Almanac -> String -> String -> Range Int -> [Range Int]
forwardRange almanac from to = go almanac path
  where
      path = getPath almanac from to
      go :: Almanac -> [(String, String)] -> Range Int -> [Range Int]
      go _ [] sr = [sr]
      go a (p : ps) sr =
        let src =  maybe [] (map getSourceRange) (M.lookup p a)
            intr = concatMap (\src' -> intersection [sr] [src']) src
            diff = difference [sr] src
            intr' = map (fmap (forward' a p)) intr
            sr' = mergeRanges diff ++ intr'
          in mergeRanges $ concatMap (go a ps) sr'

solvePart01 :: [String] -> Int
solvePart01 input =
  let (ss, a) = parseAlmanac input
      f = forward a "seed" "location"
   in minimum $ map f ss

seedsToRanges :: Seeds -> [Range Int]
seedsToRanges [] = []
seedsToRanges [_] = error "Not in pairs"
seedsToRanges (s1 : s2 : rest) =
  s1 +=* (s1 + s2) : seedsToRanges rest

solvePart02 :: [String] -> Int
solvePart02 input =
  let (ss, a) = parseAlmanac input
      ss' = seedsToRanges ss
      locations = concatMap (forwardRange a "seed" "location") ss'
    in minimum $ fromRanges locations
