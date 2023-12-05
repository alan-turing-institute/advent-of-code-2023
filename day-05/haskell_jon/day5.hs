{-# LANGUAGE OverloadedStrings #-}

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

data Card = Card {cardId :: Int, winningNumbers :: Set Int, yourNumbers :: Set Int}
  deriving (Show)

yourWinners :: Card -> Set Int
yourWinners card = S.intersection (yourNumbers card) (winningNumbers card)

winners :: Card -> Int
winners card = S.size (yourWinners card)

score :: Card -> Int
score card = case winners card of
  0 -> 0
  n -> 2 ^ (n - 1)

-- * Parsing

type Parser = Parsec Void Text

lx :: Parser a -> Parser a
lx = L.lexeme (L.space space1 empty empty)

pCard :: Parser Card
pCard = do
  _ <- lx $ string "Card"
  cardId <- L.decimal
  _ <- lx $ char ':'
  winners <- S.fromList <$> some (lx L.decimal)
  _ <- lx $ char '|'
  yours <- S.fromList <$> some (lx L.decimal)
  return $ Card cardId winners yours

parseLine :: Text -> Card
parseLine t =
  case parse pCard "" t of
    Left err -> error $ errorBundlePretty err
    Right res -> res

-- * Main

part1 :: [Card] -> Int
part1 = sum . map score

part2 :: [Card] -> Int
part2 cards =
  -- IntMap Int = cardId -> number of extra copies of that card
  -- Int = accumulator of total numbers of card
  let addCopy copies m cardId = IM.insertWith (+) cardId copies m
      addCopies copies initialCardId nWins m =
        foldl' (addCopy copies) m [initialCardId + 1 .. initialCardId + nWins]
      f :: (IntMap Int, Int) -> Card -> (IntMap Int, Int)
      f (m, total) card =
        let copies = 1 + IM.findWithDefault 0 (cardId card) m
         in ( addCopies copies (cardId card) (winners card) m,
              copies + total
            )
   in snd $ foldl' f (IM.empty, 0) cards

main :: IO ()
main = do
  inputLines <- T.lines <$> T.readFile "input.txt"
  let cards = map parseLine inputLines
  print $ part1 cards
  print $ part2 cards
