{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.List (transpose)
import Data.List.Extra (groupOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

data Square = Ash | Rock deriving (Eq, Ord, Show)

type Grid = [[Square]]

data Part = Part1 | Part2 deriving (Eq, Ord, Show)

-- | Number of differences between two lists
nDiffsBetween :: (Eq a) => [a] -> [a] -> Int
nDiffsBetween xs ys = length $ filter id $ zipWith (/=) xs ys

getHorizontalMirrorPos :: Part -> Grid -> Int
getHorizontalMirrorPos part grid =
  -- Find two consecutive lines that are the same (for part 1), or differ by at most 1 square (for part 2)
  let indexedLinesWithNext = zip3 [1 ..] grid (tail grid)
      fst3 (a, _, _) = a
      potentialIndices =
        case part of
          Part1 -> map fst3 $ filter (\(_, l1, l2) -> l1 == l2) indexedLinesWithNext
          Part2 -> map fst3 $ filter (\(_, l1, l2) -> nDiffsBetween l1 l2 <= 1) indexedLinesWithNext
   in -- Then we can verify them in more detail to check that they are indeed
      -- valid mirrors.
      case potentialIndices of
        [] -> 0
        ns -> case mapMaybe (isValidHorizontalMirror part grid) ns of
          [] -> 0
          n : _ -> n

-- Verifies whether the given index is a valid horizontal mirror (i.e. a perfect
-- mirror for Part 1, or a mirror with one smudge for Part 2). Returns Just the
-- index if it is, otherwise Nothing.
--
-- Note that sum (zipWith nDiffsBetween ...) is slightly inefficient, because it
-- calculates the number of differences across the entire grid. In principle, we
-- only need to check whether the number of differences exceeds a threshold, and
-- once the threshold is exceeded we can stop checking. This could be done with
-- an appropriate short-circuiting call to foldr. But in practice, the grids are
-- small enough that this is not a problem.
isValidHorizontalMirror :: Part -> Grid -> Int -> Maybe Int
isValidHorizontalMirror part grid n =
  let (top, bottom) = splitAt n grid
   in case (part, sum $ zipWith nDiffsBetween (reverse top) bottom) of
        (Part1, 0) -> Just n
        (Part2, 1) -> Just n
        _anythingElse -> Nothing

getVerticalMirrorPos :: Part -> Grid -> Int
getVerticalMirrorPos p = getHorizontalMirrorPos p . transpose

summarise :: Part -> Grid -> Int
summarise part grid = v + 100 * h
  where
    -- We have implicitly assumed here that the input is set up such that either
    -- v or h is nonzero, and the other is always zero (i.e. there is only one
    -- possible mirror). I think this is a reasonable assumption to make because
    -- the question does not specify what happens if this isn't the case.
    v = getVerticalMirrorPos part grid
    h = getHorizontalMirrorPos part grid

-- * Parsing

type Parser = Parsec Void Text

pSquare :: Parser Square
pSquare = (Ash <$ char '.') <|> (Rock <$ char '#')

pLine :: Parser [Square]
pLine = some pSquare <* newline

pGrid :: Parser Grid
pGrid = some pLine <* (void newline <|> eof)

parseWith :: Parser a -> Text -> a
parseWith p t =
  case parse p "" t of
    Left e -> error $ errorBundlePretty e
    Right r -> r

-- * Main

part1 :: [Grid] -> Int
part1 grids = sum $ map (summarise Part1) grids

part2 :: [Grid] -> Int
part2 grids = sum $ map (summarise Part2) grids

main :: IO ()
main = do
  grids <- parseWith (some pGrid) <$> T.readFile "input.txt"
  print $ part1 grids
  print $ part2 grids
