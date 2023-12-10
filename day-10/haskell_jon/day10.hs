{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.List (foldl')
import Data.List.Extra (groupOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

data Direction = DUp | DDown | DLeft | DRight
  deriving (Show, Eq)

opp :: Direction -> Direction
opp DUp = DDown
opp DDown = DUp
opp DLeft = DRight
opp DRight = DLeft

data Tile = Ground | Starting | Tile !Direction !Direction
  deriving (Show, Eq)

type Grid = Map (Int, Int) Tile

moveCoord :: (Int, Int) -> Direction -> (Int, Int)
moveCoord (x, y) DUp = (x, y - 1)
moveCoord (x, y) DDown = (x, y + 1)
moveCoord (x, y) DLeft = (x - 1, y)
moveCoord (x, y) DRight = (x + 1, y)

nextDirection ::
  -- | Tile we're on now
  Tile ->
  -- | The previous direction we moved in. If coming from the tile above, this is DDown
  Direction ->
  -- | The next direction we should move in
  Direction
nextDirection (Tile d1 d2) from = if d1 == opp from then d2 else d1

move :: Grid -> (Direction, (Int, Int)) -> (Direction, (Int, Int))
move grid (currentDirection, (curX, curY)) =
  let nextCoord = moveCoord (curX, curY) currentDirection
      nextTile = grid M.! nextCoord
      nextDir = nextDirection nextTile currentDirection
   in (nextDir, nextCoord)

-- * Parsing

withIndex :: Parser [a] -> Parser [(Int, a)]
withIndex p = zip [0 ..] <$> p

type Parser = Parsec Void Text

pLine :: Parser [Tile]
pLine = do
  t <-
    some $
      choice
        [ Tile DUp DDown <$ char '|',
          Tile DLeft DRight <$ char '-',
          Tile DUp DRight <$ char 'L',
          Tile DUp DLeft <$ char 'J',
          Tile DDown DRight <$ char 'F',
          Tile DDown DLeft <$ char '7',
          Ground <$ char '.',
          Starting <$ char 'S'
        ]
  void newline <|> eof
  pure t

pInput :: Parser Grid
pInput = do
  rawGrid <- withIndex $ some $ withIndex pLine
  eof
  pure $ M.fromList $ concatMap (\(y, xTiles) -> [((x, y), tile) | (x, tile) <- xTiles]) rawGrid

getValidDirections :: Grid -> (Int, Int) -> [Direction]
getValidDirections grid (x, y) =
  canGoUp ++ canGoDown ++ canGoLeft ++ canGoRight
  where
    canGoUp = case M.lookup (x, y - 1) grid of
      Just (Tile d1 d2) -> [DUp | d1 == DDown || d2 == DDown]
      _anyOtherTile -> []
    canGoDown = case M.lookup (x, y + 1) grid of
      Just (Tile d1 d2) -> [DDown | d1 == DUp || d2 == DUp]
      _anyOtherTile -> []
    canGoLeft = case M.lookup (x - 1, y) grid of
      Just (Tile d1 d2) -> [DLeft | d1 == DRight || d2 == DRight]
      _anyOtherTile -> []
    canGoRight = case M.lookup (x + 1, y) grid of
      Just (Tile d1 d2) -> [DRight | d1 == DLeft || d2 == DLeft]
      _anyOtherTile -> []

replaceStarting :: Grid -> (Grid, (Int, Int))
replaceStarting grid =
  let startingPos = head [(x, y) | ((x, y), Starting) <- M.toList grid]
      validAdjacentTiles = getValidDirections grid startingPos
      newTile = case validAdjacentTiles of
        [d1, d2] -> Tile d1 d2
        _anyOtherDirections -> error "Invalid starting tile"
   in (M.insert startingPos newTile grid, startingPos)

parseInput :: Text -> (Grid, (Int, Int))
parseInput t =
  case parse pInput "" t of
    Left e -> error $ errorBundlePretty e
    Right r -> replaceStarting r

-- * Main

part1 :: Grid -> (Int, Int) -> Int
part1 grid starting =
  let initialDirection = case grid M.! starting of
        Tile d1 _ -> d1
        _anyOtherTile -> error "Invalid starting tile"
      trajectory = iterate (move grid) (initialDirection, starting)
      loopSizeExcludingStart = length $ takeWhile (\(_, crd) -> crd /= starting) $ tail trajectory
   in (loopSizeExcludingStart + 1) `div` 2

-- Version of the grid which only retains info about whether a tile is part of
-- the main loop or not

data SimplifiedTile = MainLoop | AnythingElse deriving (Show, Eq)

type SimplifiedGrid = Map (Int, Int) SimplifiedTile

simplifyGrid :: Grid -> (Int, Int) -> SimplifiedGrid
simplifyGrid grid starting =
  let initialDirection = case grid M.! starting of
        Tile d1 _ -> d1
        _anyOtherTile -> error "Invalid starting tile"
      trajectory = iterate (move grid) (initialDirection, starting)
      loopCoordinatesExcludingStart = map snd $ takeWhile (\(_, crd) -> crd /= starting) $ tail trajectory
      loopCoordinates = starting : loopCoordinatesExcludingStart
   in M.mapWithKey (\crd _ -> if crd `elem` loopCoordinates then MainLoop else AnythingElse) grid

part2 :: SimplifiedGrid -> Int
part2 grid = sum $ traceShowId $ map countCellsInLoopPerLine gridRows
  where
    gridRows = map (map snd) $ groupOn (\((x, _), _) -> x) $ M.toList grid
    countCellsInLoopPerLine :: [SimplifiedTile] -> Int
    countCellsInLoopPerLine tiles =
      let evenNumberOfWalls =
            traceShowId . even . fst $
              foldl'
                ( \(acc, prevTile) tile ->
                    if tile == MainLoop && prevTile == AnythingElse then (acc + 1, tile) else (acc, tile)
                )
                (0, AnythingElse)
                tiles
       in (\(acc, _, _) -> acc) $
            foldl'
              ( \(acc, mainLoopSegmentsSeen, prevTile) tile ->
                  case (tile, even mainLoopSegmentsSeen, prevTile) of
                    (MainLoop, _, MainLoop) -> (acc, mainLoopSegmentsSeen, tile)
                    (MainLoop, _, AnythingElse) -> (acc, mainLoopSegmentsSeen + 1, tile)
                    (AnythingElse, True, _) -> (if evenNumberOfWalls then acc else acc + 1, mainLoopSegmentsSeen, tile)
                    (AnythingElse, False, _) -> (if evenNumberOfWalls then acc + 1 else acc, mainLoopSegmentsSeen, tile)
              )
              (0, 0, AnythingElse)
              tiles

main :: IO ()
main = do
  (grid, starting) <- parseInput <$> T.readFile "example.txt"
  print $ part1 grid starting
  print $ part2 $ simplifyGrid grid starting
