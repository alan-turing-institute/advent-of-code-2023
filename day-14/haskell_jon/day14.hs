{-# LANGUAGE OverloadedStrings #-}

import Data.List (elemIndex, foldl', sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

data Square = Round | Cube | Empty deriving (Eq, Ord, Show)

data Coord = Coord !Int !Int deriving (Eq, Ord, Show)

type Grid = Map Coord Square

data Direction = North | South | East | West deriving (Eq, Ord, Show)

move :: Direction -> Grid -> Coord -> Coord
move dir grid (Coord row col) =
  let nextCoord = case dir of
        North -> Coord (row - 1) col
        South -> Coord (row + 1) col
        East -> Coord row (col + 1)
        West -> Coord row (col - 1)
   in case M.lookup nextCoord grid of
        Just Empty -> move dir grid nextCoord
        _anythingElse -> Coord row col

swapValues :: Grid -> Coord -> Coord -> Grid
swapValues grid c1 c2 =
  if c1 == c2
    then grid
    else
      let v1 = grid M.! c1
          v2 = grid M.! c2
       in M.insert c1 v2 $ M.insert c2 v1 grid

tilt :: Direction -> Grid -> Grid
tilt dir grid =
  let roundRocks = M.keys $ M.filter (== Round) grid
      -- We want to iterate through all the keys, but we do want to make sure that
      -- they are sorted properly. For example, if we are tilting north, we want
      -- to make sure we move the topmost rocks first.
      sortedRoundRocks = case dir of
        North -> sortOn (\(Coord r _) -> r) roundRocks
        South -> sortOn (\(Coord r _) -> Down r) roundRocks
        East -> sortOn (\(Coord _ c) -> Down c) roundRocks
        West -> sortOn (\(Coord _ c) -> c) roundRocks
   in foldl'
        ( \g coord ->
            let newCoord = move dir g coord
             in swapValues g coord newCoord
        )
        grid
        sortedRoundRocks

cycleNWSE :: Grid -> Grid
cycleNWSE = tilt East . tilt South . tilt West . tilt North

calcNorthStrain :: Grid -> Int
calcNorthStrain g =
  let nrows = maximum $ map (\(Coord r _) -> r) $ M.keys g
      strain :: Coord -> Int
      strain (Coord r _) = nrows + 1 - r
   in sum $ map strain $ M.keys $ M.filter (== Round) g

-- * Parsing

type Parser = Parsec Void Text

pSquare :: Parser (Coord, Square)
pSquare = do
  char <- (Round <$ char 'O') <|> (Cube <$ char '#') <|> (Empty <$ char '.')
  pos <- getSourcePos
  pure (Coord (unPos (sourceLine pos)) (unPos (sourceColumn pos) - 1), char)

pLine :: Parser [(Coord, Square)]
pLine = some pSquare <* newline

pListOfList :: Parser [[(Coord, Square)]]
pListOfList = some pLine <* eof

pGrid :: Parser Grid
pGrid = M.fromList . concat <$> pListOfList

parseWith :: Parser a -> Text -> a
parseWith p t =
  case parse p "" t of
    Left e -> error $ errorBundlePretty e
    Right r -> r

part1 :: Grid -> Int
part1 grid = calcNorthStrain $ tilt North grid

part2 :: Grid -> Int
part2 grid =
  let (i, j) = findCycle cycleNWSE grid
      period = j - i
      n = 1000000000
      -- We don't need to run n steps. We can just run
      -- n' steps, where n' === n mod period, and n' >= i.
      n' = period * (i `div` period + 1) + (n `mod` period)
   in calcNorthStrain $ iterate cycleNWSE grid !! n'

-- | findCycle f x returns the first pair of integers (i, j) such that
--      j > i >= 0,     and
--      f^i x == f^j x.
-- NOTE: this function assumes that such a cycle exists. If it doesn't
-- then this function will loop forever!
findCycle :: (Eq a) => (a -> a) -> a -> (Int, Int)
findCycle f = go [] 0
  where
    go stored n y =
      case elemIndex y stored of
        Just i -> (i, n)
        Nothing -> go (stored ++ [y]) (n + 1) (f y)

-- * Main

main :: IO ()
main = do
  grid <- parseWith pGrid <$> T.readFile "input.txt"
  print $ part1 grid
  print $ part2 grid
