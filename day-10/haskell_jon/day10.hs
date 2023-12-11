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

-- | Version of the grid which throws away all the tiles that are not part of
-- the main loop
data SimplifiedTile = MainLoop !Tile | AnythingElse deriving (Show, Eq)

type SimplifiedGrid = Map (Int, Int) SimplifiedTile

simplifyGrid :: Grid -> (Int, Int) -> SimplifiedGrid
simplifyGrid grid starting =
  let initialDirection = getInitialDirection grid starting
      trajectory = iterate (move grid) (initialDirection, starting)
      loopCoordinatesExcludingStart = map snd $ takeWhile (\(_, crd) -> crd /= starting) $ tail trajectory
      loopCoordinates = starting : loopCoordinatesExcludingStart
   in M.mapWithKey (\crd tile -> if crd `elem` loopCoordinates then MainLoop tile else AnythingElse) grid

-- | Whether we're 'inside' or 'outside' the main loop
-- Inside tells us which tile initially got us inside the loop.
-- InsideNested keeps track of both the tile that got us inside the loop, plus
-- the previous state so that we can 'restore' it when exiting an inner loop
data LoopState = Inside !Tile | InsideNested !Tile !LoopState | Outside deriving (Show, Eq)

part2 :: SimplifiedGrid -> Int
part2 grid =
  let gridRows = map (map snd) $ groupOn (\((x, _), _) -> x) $ M.toList grid
      countInnerCells :: [SimplifiedTile] -> Int
      countInnerCells tiles =
        fst $
          foldl'
            ( \(acc, state) tile ->
                case (tile, state) of
                  -- Goodness knows how I got all this correct.
                  (MainLoop TileLR, Outside) -> (acc, Inside TileLR)
                  (MainLoop TileLR, Inside _) -> (acc, Outside)
                  -- passing through a UD tile can't change the state
                  (MainLoop TileUD, st) -> (acc, st)
                  -- These are painful. Since we're traversing the grid by
                  -- columns, downwards, DR and DL are the two other tiles that
                  -- can bring us from outside to inside.
                  (MainLoop TileDR, Outside) -> (acc, Inside TileDR)
                  (MainLoop TileDL, Outside) -> (acc, Inside TileDL)
                  -- However, we also need to keep track of the previous state.
                  -- If we are already inside a loop, for example, then when
                  -- this stretch of the main loop ends we need to pop back into
                  -- the previous state...
                  (MainLoop TileDR, insideSt) -> (acc, InsideNested TileDR insideSt)
                  (MainLoop TileDL, insideSt) -> (acc, InsideNested TileDL insideSt)
                  -- ... like this.
                  (MainLoop TileUR, InsideNested TileDR st) -> (acc, st)
                  (MainLoop TileUR, InsideNested TileDL st) -> (acc, Outside)
                  -- If we started with a DR from an outside state, then a UR
                  -- would bring us back to outside.
                  (MainLoop TileUR, Inside TileDR) -> (acc, Outside)
                  -- If we started from a DL from an outside state, then a UR
                  -- would actually leave us *inside*.
                  (MainLoop TileUR, st) -> (acc, st)
                  -- The UL bits are the same as the UR bits, but mirrored.
                  (MainLoop TileUL, InsideNested TileDL st) -> (acc, st)
                  (MainLoop TileUL, InsideNested TileDR st) -> (acc, Outside)
                  (MainLoop TileUL, Inside TileDL) -> (acc, Outside)
                  (MainLoop TileUL, st) -> (acc, st)
                  -- Anything else can just be carried forward (I think).
                  (MainLoop t, Inside _) -> (acc, Inside t)
                  -- The real bits we want to count.
                  (AnythingElse, InsideNested _ _) -> (acc + 1, state)
                  (AnythingElse, Inside _) -> (acc + 1, state)
                  (AnythingElse, Outside) -> (acc, state)
            )
            (0, Outside)
            tiles
   in sum $ map countInnerCells gridRows

main :: IO ()
main = do
  (grid, starting) <- parseInput <$> T.readFile "input.txt"
  print $ part1 grid starting
  print $ part2 $ simplifyGrid grid starting
