{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

-- | No idea what happens if the list of newtypes is not static. Probably just
-- curl up and cry.
newtype Seed = Seed Int deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype Soil = Soil Int deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype Fertilizer = Fertilizer Int deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype Water = Water Int deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype Light = Light Int deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype Temperature = Temperature Int deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype Humidity = Humidity Int deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype Location = Location Int deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

-- Proxy types.
newtype Shift src dst = Shift Int deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

data Interval src = Interval
  { start :: !src,
    end :: !src
  }
  deriving (Show, Eq, Ord)

data IntervalMap src dst = IntervalMap
  { srcStart :: !src,
    srcEnd :: !src, -- start + length - 1, i.e. inclusive
    dstStart :: !dst
  }
  deriving (Show, Eq, Ord)

shift :: (Integral src, Integral dst) => IntervalMap src dst -> Shift src dst
shift m = fromIntegral (dstStart m) - fromIntegral (srcStart m)

shiftInterval :: (Integral src, Integral dst) => Shift src dst -> Interval src -> Interval dst
shiftInterval shift (Interval s e) =
  let shift' = fromIntegral shift
   in Interval (fromIntegral s + shift') (fromIntegral e + shift')

shiftIntervalUsing :: (Integral src, Integral dst) => IntervalMap src dst -> Interval src -> Interval dst
shiftIntervalUsing im = shiftInterval (shift im)

isValid :: (Ord src) => Interval src -> Bool
isValid (Interval s e) = s <= e

remapOverlap ::
  (Integral src, Integral dst) =>
  IntervalMap src dst ->
  Interval src ->
  ([Interval src], [Interval dst])
remapOverlap im@(IntervalMap ss se ds) i@(Interval s e) =
  if se < s || ss > e -- no overlap
    then ([i], [])
    else
      let (srcs, dsts) = case (ss < s, se > e) of
            (True, True) -> ([], [shiftIntervalUsing im i])
            (False, False) ->
              ( [ Interval s (ss - 1),
                  Interval (se + 1) e
                ],
                [shiftIntervalUsing im (Interval ss se)]
              )
            (True, False) ->
              ( [Interval (se + 1) e],
                [shiftIntervalUsing im (Interval s se)]
              )
            (False, True) ->
              ( [Interval s (ss - 1)],
                [shiftIntervalUsing im (Interval ss e)]
              )
       in (filter isValid srcs, filter isValid dsts)

transformWith ::
  (Integral src, Integral dst) =>
  IntervalMap src dst ->
  [Interval src] ->
  ([Interval src], [Interval dst])
transformWith m srcs =
  let f (srcs, dsts) i = (srcs ++ srcs', dsts ++ dsts')
        where
          (srcs', dsts') = remapOverlap m i
   in foldl' f ([], []) srcs

transformWith' ::
  (Integral src, Integral dst) =>
  [IntervalMap src dst] ->
  [Interval src] ->
  ([Interval src], [Interval dst])
transformWith' maps is =
  let f (srcs, dsts) m = (srcs', dsts ++ dsts')
        where
          (srcs', dsts') = transformWith m srcs
   in foldl' f (is, []) maps

moveSrcToDst :: (Integral src, Integral dst) => ([Interval src], [Interval dst]) -> [Interval dst]
moveSrcToDst (is, os) = map (shiftInterval 0) is ++ os

transform ::
  (Integral src, Integral dst) =>
  [IntervalMap src dst] ->
  [Interval src] ->
  [Interval dst]
transform maps = moveSrcToDst . transformWith' maps

-- * Parsing

type Parser = Parsec Void Text

lx :: Parser a -> Parser a
lx = L.lexeme (L.space space1 empty empty)

pSeedsPart1 :: Parser [Interval Seed]
pSeedsPart1 = do
  _ <- lx $ string "seeds:"
  seeds <- many $ lx L.decimal
  pure $ map (\s -> Interval s s) seeds

pSeedsPart2 :: Parser [Interval Seed]
pSeedsPart2 = do
  _ <- lx $ string "seeds:"
  many $ do
    srcStart <- lx L.decimal
    srcLength <- lx L.decimal
    pure $ Interval srcStart (srcStart + srcLength - 1)

pMap :: (Integral src, Integral dst) => Text -> Parser [IntervalMap src dst]
pMap t = do
  _ <- lx $ string t
  many $ do
    dstStart <- lx L.decimal
    srcStart <- lx L.decimal
    srcLength <- lx L.decimal
    let srcEnd = srcStart + srcLength - 1
    pure $ IntervalMap srcStart srcEnd dstStart

data ParsedInput = ParsedInput
  { seeds :: ![Interval Seed],
    seedSoilMaps :: ![IntervalMap Seed Soil],
    soilFertilizerMaps :: ![IntervalMap Soil Fertilizer],
    fertilizerWaterMaps :: ![IntervalMap Fertilizer Water],
    waterLightMaps :: ![IntervalMap Water Light],
    lightTemperatureMaps :: ![IntervalMap Light Temperature],
    temperatureHumidityMaps :: ![IntervalMap Temperature Humidity],
    humidityLocationMaps :: ![IntervalMap Humidity Location]
  }
  deriving (Show, Eq, Ord)

data Part = Part1 | Part2 deriving (Eq)

pInput :: Part -> Parser ParsedInput
pInput pt = do
  seeds <- if pt == Part1 then pSeedsPart1 else pSeedsPart2
  seedSoilMaps <- pMap "seed-to-soil map:"
  soilFertilizerMaps <- pMap "soil-to-fertilizer map:"
  fertilizerWaterMaps <- pMap "fertilizer-to-water map:"
  waterLightMaps <- pMap "water-to-light map:"
  lightTemperatureMaps <- pMap "light-to-temperature map:"
  temperatureHumidityMaps <- pMap "temperature-to-humidity map:"
  humidityLocationMaps <- pMap "humidity-to-location map:"
  pure $ ParsedInput {..}

parseInput :: Part -> Text -> ParsedInput
parseInput pt t =
  case parse (pInput pt) "" t of
    Left err -> error $ errorBundlePretty err
    Right res -> res

-- * Main

getLocation :: ParsedInput -> [Interval Location]
getLocation (ParsedInput {..}) =
  transform humidityLocationMaps
    . transform temperatureHumidityMaps
    . transform lightTemperatureMaps
    . transform waterLightMaps
    . transform fertilizerWaterMaps
    . transform soilFertilizerMaps
    . transform seedSoilMaps
    $ seeds

getMinimumLocation :: ParsedInput -> Location
getMinimumLocation p =
  let locations = getLocation p
   in minimum $ map start locations

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  let part1Input = parseInput Part1 input
  print $ getMinimumLocation part1Input
  let part2Input = parseInput Part2 input
  print $ getMinimumLocation part2Input
