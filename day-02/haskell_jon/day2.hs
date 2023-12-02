{-# LANGUAGE OverloadedStrings #-}

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty.Extra (maximum1)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

data RGB = RGB
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Show)

instance Semigroup RGB where
  (<>) (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)

instance Monoid RGB where
  mempty = RGB 0 0 0

data Game = Game
  { gameId :: Int,
    revealed :: NonEmpty RGB
  }
  deriving (Show)

satisfies :: Game -> RGB -> Bool
satisfies game (RGB r g b) =
  all (\(RGB r' g' b') -> r' <= r && g' <= g && b' <= b) (revealed game)

power :: Game -> Int
power (Game _ rv) =
  let rmax = maximum1 . NE.map red $ rv
      gmax = maximum1 . NE.map green $ rv
      bmax = maximum1 . NE.map blue $ rv
   in rmax * gmax * bmax

-- * Parsing

type Parser = Parsec Void Text

-- Modifies parsers to additionally consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 empty empty)

-- Parse e.g. "4 red, 3 green, 2 blue" -> RGB 4 3 2
prgb :: Parser RGB
prgb = do
  combis <- some $ do
    n <- lexeme L.decimal
    withColor <-
      lexeme
        ( choice
            [ (\r -> RGB r 0 0) <$ string "red",
              (\g -> RGB 0 g 0) <$ string "green",
              (\b -> RGB 0 0 b) <$ string "blue"
            ]
        )
    _ <- optional (lexeme (char ','))
    pure $ withColor n
  _ <- optional (lexeme (char ';'))
  pure $ mconcat combis

-- Parse a line of the input
pgame :: Parser Game
pgame = do
  _ <- lexeme (string "Game")
  gameId <- L.decimal
  _ <- lexeme (char ':')
  rev1 <- prgb
  revRest <- some prgb
  eof
  pure $ Game gameId (rev1 NE.:| revRest)

-- Run the parser (and error if it fails)
parseLine :: Text -> Game
parseLine line =
  case parse pgame "" line of
    Left err -> error $ errorBundlePretty err
    Right game -> game

-- * Main bits

part1 :: [Game] -> Int
part1 games = sum $ map gameId $ filter (`satisfies` RGB 12 13 14) games

part2 :: [Game] -> Int
part2 games = sum $ map power games

main :: IO ()
main = do
  inputLines <- T.lines <$> T.readFile "input.txt"
  let games = map parseLine inputLines
  print $ part1 games
  print $ part2 games
