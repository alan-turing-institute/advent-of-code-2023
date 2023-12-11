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

data Tile = Ground | Starting | TileUD | TileLR | TileUR | TileUL | TileDR | TileDL
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
nextDirection TileUD DUp = DUp
nextDirection TileUD DDown = DDown
nextDirection TileLR DLeft = DLeft
nextDirection TileLR DRight = DRight
nextDirection TileUR DDown = DRight
nextDirection TileUR DLeft = DUp
nextDirection TileUL DDown = DLeft
nextDirection TileUL DRight = DUp
nextDirection TileDR DUp = DRight
nextDirection TileDR DLeft = DDown
nextDirection TileDL DUp = DLeft
nextDirection TileDL DRight = DDown
nextDirection _anyOtherTile _anyOtherDirection = error "Invalid tile/direction combination"

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
        [ TileUD <$ char '|',
          TileLR <$ char '-',
          TileUR <$ char 'L',
          TileUL <$ char 'J',
          TileDR <$ char 'F',
          TileDL <$ char '7',
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
      Just tile -> [DUp | tile `elem` [TileUD, TileDL, TileDR]]
      _anyOtherTile -> []
    canGoDown = case M.lookup (x, y + 1) grid of
      Just tile -> [DDown | tile `elem` [TileUD, TileUL, TileUR]]
      _anyOtherTile -> []
    canGoLeft = case M.lookup (x - 1, y) grid of
      Just tile -> [DLeft | tile `elem` [TileLR, TileUR, TileDR]]
      _anyOtherTile -> []
    canGoRight = case M.lookup (x + 1, y) grid of
      Just tile -> [DRight | tile `elem` [TileLR, TileUL, TileDL]]
      _anyOtherTile -> []

replaceStarting :: Grid -> (Grid, (Int, Int))
replaceStarting grid =
  let startingPos = head [(x, y) | ((x, y), Starting) <- M.toList grid]
      validAdjacentTiles = getValidDirections grid startingPos
      newTile = case validAdjacentTiles of
        [DUp, DDown] -> TileUD
        [DLeft, DRight] -> TileLR
        [DUp, DRight] -> TileUR
        [DUp, DLeft] -> TileUL
        [DDown, DRight] -> TileDR
        [DDown, DLeft] -> TileDL
        _anyOtherDirections -> error "Invalid starting tile"
   in (M.insert startingPos newTile grid, startingPos)

parseInput :: Text -> (Grid, (Int, Int))
parseInput t =
  case parse pInput "" t of
    Left e -> error $ errorBundlePretty e
    Right r -> replaceStarting r

-- * Main

getInitialDirection :: Grid -> (Int, Int) -> Direction
getInitialDirection grid (x, y) =
  case grid M.! (x, y) of
    -- Arbitrarily pick one of the two directions
    TileUD -> DUp
    TileLR -> DLeft
    TileUR -> DUp
    TileUL -> DUp
    TileDR -> DDown
    TileDL -> DDown
    _anyOtherTile -> error "Invalid starting tile"

part1 :: Grid -> (Int, Int) -> Int
part1 grid starting =
  let initialDirection = getInitialDirection grid starting
      trajectory = iterate (move grid) (initialDirection, starting)
      loopSizeExcludingStart = length $ takeWhile (\(_, crd) -> crd /= starting) $ tail trajectory
   in (loopSizeExcludingStart + 1) `div` 2

-- Version of the grid which only retains info about whether a tile is part of
-- the main loop or not

data SimplifiedTile = MainLoop Tile | AnythingElse deriving (Show, Eq)

type SimplifiedGrid = Map (Int, Int) SimplifiedTile

simplifyGrid :: Grid -> (Int, Int) -> SimplifiedGrid
simplifyGrid grid starting =
  let initialDirection = getInitialDirection grid starting
      trajectory = iterate (move grid) (initialDirection, starting)
      loopCoordinatesExcludingStart = map snd $ takeWhile (\(_, crd) -> crd /= starting) $ tail trajectory
      loopCoordinates = starting : loopCoordinatesExcludingStart
   in M.mapWithKey (\crd tile -> if crd `elem` loopCoordinates then MainLoop tile else AnythingElse) grid

-- | Whether we're 'inside' or 'outside' the main loop
data LoopState = Inside Tile | InsideNested Tile LoopState | Outside deriving (Show, Eq)

part2 :: SimplifiedGrid -> Int
part2 grid =
  let gridRows = map (map snd) $ groupOn (\((x, _), _) -> x) $ M.toList grid
      countInnerCells :: [SimplifiedTile] -> Int
      countInnerCells tiles =
        fst $
          foldl'
            ( \(acc, state) tile ->
                case (tile, state) of
                  -- LR/UD, same as above, this part just keeps track of the
                  -- state
                  (MainLoop TileLR, Outside) -> (acc, Inside TileLR)
                  (MainLoop TileLR, Inside _) -> (acc, Outside)
                  (MainLoop TileUD, st) -> (acc, st)
                  -- Beginning of a DR/DL
                  (MainLoop TileDR, Outside) -> (acc, Inside TileDR)
                  (MainLoop TileDL, Outside) -> (acc, Inside TileDL)
                  (MainLoop TileDR, insideSt) -> (acc, InsideNested TileDR insideSt)
                  (MainLoop TileDL, insideSt) -> (acc, InsideNested TileDL insideSt)
                  -- Ending of a DR/DL
                  (MainLoop TileUR, InsideNested TileDR st) -> (acc, st)
                  (MainLoop TileUR, InsideNested TileDL st) -> (acc, Outside)
                  (MainLoop TileUR, Inside TileDR) -> (acc, Outside)
                  (MainLoop TileUR, st) -> (acc, st)
                  (MainLoop TileUL, InsideNested TileDL st) -> (acc, st)
                  (MainLoop TileUL, InsideNested TileDR st) -> (acc, Outside)
                  (MainLoop TileUL, Inside TileDL) -> (acc, Outside)
                  (MainLoop TileUL, st) -> (acc, st)
                  (MainLoop t, Inside _) -> (acc, Inside t)
                  -- The real bits we want to count
                  (AnythingElse, Inside _) -> (acc + 1, state)
                  (AnythingElse, Outside) -> (acc, state)
            )
            (0, Outside)
            tiles
      innerCells = traceShowId $ map countInnerCells gridRows
   in sum innerCells

main :: IO ()
main = do
  (grid, starting) <- parseInput <$> T.readFile "input.txt"
  print $ part1 grid starting
  print $ part2 $ simplifyGrid grid starting
