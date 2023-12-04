{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Either (partitionEithers)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Debug.Trace (traceShow)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

data Coord = Coord
  { x :: Int,
    y :: Int
  }
  deriving (Show)

data PartNum = PartNum
  { -- | The number itself
    n :: Int,
    -- | The coordinate of the first digit
    c :: Coord
  }
  deriving (Show)

data Symbol = Symbol
  { ch :: Char,
    cd :: Coord
  }
  deriving (Show)

-- | Get the number of digits in an integer
getLength :: Int -> Int
getLength n
  | n < 0 = error "Negative number"
  | n < 10 = 1
  | otherwise = 1 + getLength (n `div` 10)

-- | Determine whether a part number is next to a symbol
borders :: PartNum -> Symbol -> Bool
borders (PartNum n (Coord xp yp)) (Symbol _ (Coord xs ys)) =
  let yp' = yp + getLength n - 1
   in xs >= xp - 1 && xs <= xp + 1 && ys >= yp - 1 && ys <= yp' + 1

-- * Parsing

type Parser = Parsec Void Text

pcoord :: Parser Coord
pcoord = do
  pos <- getSourcePos
  pure $ Coord (unPos $ sourceLine pos) (unPos $ sourceColumn pos)

pFiller :: Parser ()
pFiller = void (char '.') <|> space1

pSymb :: Parser Symbol
pSymb = do
  ch <- asciiChar
  Coord x y <- pcoord
  pure $ Symbol ch (Coord x (y - 1))

pNum :: Parser PartNum
pNum = do
  n <- L.decimal
  Coord x y <- pcoord
  pure $ PartNum n (Coord x (y - getLength n))

-- | Parse the grid into a list of PartNums and Symbols
pGrid :: Parser ([PartNum], [Symbol])
pGrid = do
  void $ many pFiller
  res <- some $ ((Left <$> pNum) <|> (Right <$> pSymb)) <* many pFiller
  eof
  pure $ partitionEithers res

parseInput :: Text -> ([PartNum], [Symbol])
parseInput t =
  case parse pGrid "" t of
    Left err -> error $ errorBundlePretty err
    Right res -> res

-- * Main

part1 :: ([PartNum], [Symbol]) -> Int
part1 (partnums, symbols) =
  let partnums' = filter (\p -> any (borders p) symbols) partnums
   in sum $ map n partnums'

part2 :: ([PartNum], [Symbol]) -> Int
part2 (partnums, symbols) =
  let gears = filter (\s -> ch s == '*') symbols
      ratio :: Symbol -> Int
      ratio g = case filter (`borders` g) partnums of
        [p1, p2] -> n p1 * n p2
        _anyOtherLength -> 0
   in sum $ map ratio gears

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  let grid = parseInput input
  print $ part1 grid
  print $ part2 grid
