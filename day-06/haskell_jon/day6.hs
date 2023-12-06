{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

data Race = Race {time :: !Int, recordDist :: !Int} deriving (Show, Eq, Ord)

-- Brute force
getWaysToBeat :: Race -> Int
getWaysToBeat r =
  let possibleDistances = [i * (time r - i) | i <- [0 .. time r]]
   in length $ filter (> recordDist r) possibleDistances

-- Solve the quadratic equation x^2 - tx + d = 0, where t = time r and d =
-- recordDist r. Then filter to find the values of x that are integers and lie
-- within the range [0, time r].
getWaysToBeat' :: Race -> Int
getWaysToBeat' r =
  let t = fromIntegral $ time r
      d = fromIntegral $ recordDist r
      x1 = ceiling $ (t - sqrt (t * t - 4 * d)) / 2
      x2 = floor $ (t + sqrt (t * t - 4 * d)) / 2
      x1max = max x1 0
      x2min = min x2 (time r)
   in x2min - x1max + 1

-- * Parsing

type Parser = Parsec Void Text

lx :: Parser a -> Parser a
lx = L.lexeme (L.space space1 empty empty)

pInput :: Parser [Race]
pInput = do
  _ <- lx $ string "Time:"
  times <- some $ lx L.decimal
  _ <- lx $ string "Distance:"
  recordDists <- some $ lx L.decimal
  pure $ zipWith Race times recordDists

parseInput :: Text -> [Race]
parseInput t =
  case parse pInput "" t of
    Left err -> error $ errorBundlePretty err
    Right res -> res

-- * Main

main :: IO ()
main = do
  races1 <- parseInput <$> T.readFile "input.txt"
  print $ product $ map getWaysToBeat races1
  print $ product $ map getWaysToBeat' races1
  races2 <- parseInput . T.filter (not . isSpace) <$> T.readFile "input.txt"
  print $ product $ map getWaysToBeat races2
  print $ product $ map getWaysToBeat' races2
