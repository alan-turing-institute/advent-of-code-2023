{-# LANGUAGE OverloadedStrings #-}

import Data.List (group, nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

data Card = N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | Ten | J | Queen | King | Ace
  deriving (Eq, Ord, Show)

-- Same type, but with a different Ord instance where J is always the lowest
newtype CardJ = CardJ Card
  deriving (Eq, Show)

instance Ord CardJ where
  compare (CardJ J) (CardJ J) = EQ
  compare (CardJ J) _ = LT
  compare _ (CardJ J) = GT
  compare (CardJ c1) (CardJ c2) = compare c1 c2

newtype Hand = Hand (Card, Card, Card, Card, Card)
  deriving (Eq, Show)

newtype HandJ = HandJ (CardJ, CardJ, CardJ, CardJ, CardJ)
  deriving (Eq, Show)

instance Ord Hand where
  compare (Hand cs1) (Hand cs2) =
    case compare (getType (Hand cs1)) (getType (Hand cs2)) of
      LT -> LT
      GT -> GT
      EQ -> compare cs1 cs2

instance Ord HandJ where
  compare (HandJ cs1) (HandJ cs2) =
    case compare (getTypeJ (HandJ cs1)) (getTypeJ (HandJ cs2)) of
      LT -> LT
      GT -> GT
      EQ -> compare cs1 cs2

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Ord, Show)

getType :: Hand -> HandType
getType (Hand (a, b, c, d, e)) =
  let cardGroups = group $ sort [a, b, c, d, e]
      groupSizes = sort $ map length cardGroups
   in case groupSizes of
        [1, 1, 1, 1, 1] -> HighCard
        [1, 1, 1, 2] -> OnePair
        [1, 2, 2] -> TwoPair
        [1, 1, 3] -> ThreeOfAKind
        [2, 3] -> FullHouse
        [1, 4] -> FourOfAKind
        [5] -> FiveOfAKind
        _anyOtherCombination -> error "(╯°□°)╯︵ ┻━┻"

getTypeJ :: HandJ -> HandType
getTypeJ (HandJ (CardJ a, CardJ b, CardJ c, CardJ d, CardJ e)) =
  let nonJokerCards = filter (/= J) [a, b, c, d, e]
      n = length nonJokerCards
   in case n of
        0 -> FiveOfAKind
        5 -> getType $ Hand (a, b, c, d, e)
        _ ->
          let uniqueNonJokerCards = nub nonJokerCards -- the possible cards the Joker can turn into
              possibleHands = map (\c -> nonJokerCards ++ replicate (5 - n) c) uniqueNonJokerCards
              possibleHandTypes = map (getType . Hand . (\[a, b, c, d, e] -> (a, b, c, d, e))) possibleHands
           in maximum possibleHandTypes

-- * Parsing

type Parser = Parsec Void Text

lx :: Parser a -> Parser a
lx = L.lexeme (L.space space1 empty empty)

pCard :: Parser Card
pCard =
  choice
    [ N1 <$ char '1',
      N2 <$ char '2',
      N3 <$ char '3',
      N4 <$ char '4',
      N5 <$ char '5',
      N6 <$ char '6',
      N7 <$ char '7',
      N8 <$ char '8',
      N9 <$ char '9',
      Ten <$ char 'T',
      J <$ char 'J',
      Queen <$ char 'Q',
      King <$ char 'K',
      Ace <$ char 'A'
    ]

pHand :: Parser Hand
pHand = do
  a <- pCard
  b <- pCard
  c <- pCard
  d <- pCard
  e <- pCard
  pure $ Hand (a, b, c, d, e)

pHandJ :: Parser HandJ
pHandJ = do
  a <- pCard
  b <- pCard
  c <- pCard
  d <- pCard
  e <- pCard
  pure $ HandJ (CardJ a, CardJ b, CardJ c, CardJ d, CardJ e)

type Bid = Int

pInput1 :: Parser [(Hand, Bid)]
pInput1 = some $ do
  hand <- lx pHand
  bid <- lx L.decimal
  pure (hand, bid)

pInput2 :: Parser [(HandJ, Bid)]
pInput2 = some $ do
  hand <- lx pHandJ
  bid <- lx L.decimal
  pure (hand, bid)

parseInput1 :: Text -> [(Hand, Bid)]
parseInput1 t =
  case parse pInput1 "" t of
    Left err -> error $ errorBundlePretty err
    Right res -> res

parseInput2 :: Text -> [(HandJ, Bid)]
parseInput2 t =
  case parse pInput2 "" t of
    Left err -> error $ errorBundlePretty err
    Right res -> res

-- * Main

main :: IO ()
main = do
  handsAndBids1 <- parseInput1 <$> T.readFile "input.txt"
  print $ sum $ zipWith (\rank (_, bid) -> rank * bid) [1 ..] (sort handsAndBids1)
  handsAndBids2 <- parseInput2 <$> T.readFile "input.txt"
  print $ sum $ zipWith (\rank (_, bid) -> rank * bid) [1 ..] (sort handsAndBids2)
