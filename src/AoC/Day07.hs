module AoC.Day07 where

import AoC (readLines)
import Data.Function (on)
import Data.List (sort, sortBy, uncons)
import qualified Data.Map as M

testInput :: IO [String]
testInput = readLines "data/Day07/test.txt"

dataInput :: IO [String]
dataInput = readLines "data/Day07/data.txt"

data Hand = Hand
  { getCards :: [Card],
    getBid :: Int
  }
  deriving (Eq, Show)

instance Ord Hand where
  compare h1@(Hand c1 _) h2@(Hand c2 _) = go cmp
    where
      k1 = fromEnum (handType h1) : map fromEnum c1
      k2 = fromEnum (handType h2) : map fromEnum c2
      cmp = zipWith compare k1 k2
      go :: [Ordering] -> Ordering
      go [] = EQ
      go [x] = x
      go (EQ : xs) = go xs
      go (x : _) = x

data Card = Card Char
  deriving (Eq, Show)

instance Enum Card where
  toEnum 2 = Card '2'
  toEnum 3 = Card '3'
  toEnum 4 = Card '4'
  toEnum 5 = Card '5'
  toEnum 6 = Card '6'
  toEnum 7 = Card '7'
  toEnum 8 = Card '8'
  toEnum 9 = Card '9'
  toEnum 10 = Card 'T'
  toEnum 11 = Card 'J'
  toEnum 12 = Card 'Q'
  toEnum 13 = Card 'K'
  toEnum 14 = Card 'A'
  toEnum _ = Card '*'
  fromEnum (Card '2') = 2
  fromEnum (Card '3') = 3
  fromEnum (Card '4') = 4
  fromEnum (Card '5') = 5
  fromEnum (Card '6') = 6
  fromEnum (Card '7') = 7
  fromEnum (Card '8') = 8
  fromEnum (Card '9') = 9
  fromEnum (Card 'T') = 10
  fromEnum (Card 'J') = 11
  fromEnum (Card 'Q') = 12
  fromEnum (Card 'K') = 13
  fromEnum (Card 'A') = 14
  fromEnum (Card _) = 0

instance Ord Card where
  compare c1 c2 = compare (fromEnum c1) (fromEnum c2)

data HandType
  = FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPairs
  | OnePair
  | HighCard
  deriving (Eq, Show)

instance Enum HandType where
  toEnum 6 = FiveOfAKind
  toEnum 5 = FourOfAKind
  toEnum 4 = FullHouse
  toEnum 3 = ThreeOfAKind
  toEnum 2 = TwoPairs
  toEnum 1 = OnePair
  toEnum 0 = HighCard
  toEnum _ = error $ "Not a HandKind"

  fromEnum FiveOfAKind = 6
  fromEnum FourOfAKind = 5
  fromEnum FullHouse = 4
  fromEnum ThreeOfAKind = 3
  fromEnum TwoPairs = 2
  fromEnum OnePair = 1
  fromEnum HighCard = 0

instance Ord HandType where
  compare h1 h2 = compare (fromEnum h1) (fromEnum h2)

parseInput :: [String] -> [Hand]
parseInput = map parseRow
  where
    parseRow :: String -> Hand
    parseRow s = Hand (map Card cs) (read bid)
      where
        cs : bid : _ = words s

joker :: Card
joker = Card '*'

jack :: Card
jack = Card 'J'

getCounters :: Hand -> M.Map Card Int
getCounters (Hand cs _) =
  M.fromListWith (+) $ zipWith (,) cs (repeat 1)

replaceCounts :: Card -> Card -> M.Map Card Int -> M.Map Card Int
replaceCounts from to counters =
  let (cBefore, fCount, cAfter) = M.splitLookup from counters
      counters' = M.union cBefore cAfter
   in case fCount of
        Nothing -> counters'
        Just count -> M.insertWith (+) to count counters'

handType :: Hand -> HandType
handType hand =
  case sortedCounts of
    [5] -> FiveOfAKind
    [4, 1] -> FourOfAKind
    [3, 2] -> FullHouse
    3 : _ -> ThreeOfAKind
    [2, 2, 1] -> TwoPairs
    2 : _ -> OnePair
    _ -> HighCard
  where
    counts = getCounters hand
    jokerC = M.lookup joker counts
    topC =
      uncons $
        sortBy (flip compare `on` snd) $
          M.toList $
            M.delete joker counts
    counts' = case (jokerC, topC) of
      (Nothing, _) -> counts
      (_, Nothing) -> counts
      (Just _, Just ((topKey, _), _)) ->
        replaceCounts joker topKey counts
    sortedCounts =
      sortBy (flip compare) $ M.elems counts'

solvePart01 :: [String] -> Int
solvePart01 =
  sum
    . zipWith (*) [1 ..]
    . map getBid
    . sort
    . parseInput

replaceJoker :: Hand -> Hand
replaceJoker (Hand cs bid) = Hand cs' bid
  where
    cs' =
      map
        ( \c ->
            if c == jack
              then joker
              else c
        )
        cs

solvePart02 :: [String] -> Int
solvePart02 =
  sum
    . zipWith (*) [1 ..]
    . map getBid
    . sort
    . map replaceJoker
    . parseInput