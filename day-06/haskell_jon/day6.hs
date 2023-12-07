{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
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
-- recordDist r. Assume the two roots are x1 and x2. Then find the smallest
-- integer larger than x1 (i.e. floor x1 + 1) and the largest integer smaller
-- than x2 (i.e. ceiling x2 - 1), and count the number of integers between them.
getWaysToBeat' :: Race -> Int
getWaysToBeat' r =
  let t = fromIntegral $ time r
      d = fromIntegral $ recordDist r
      discrim = sqrt (t * t - 4 * d)
      x1 = floor $ ((t - discrim) / 2) + 1
      x2 = ceiling $ ((t + discrim) / 2) - 1
   in x2 - x1 + 1

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
  -- Ugly. Ugly ugly.
  when (length times /= length recordDists) $
    error "Number of times and record distances must be equal"
  pure $ zipWith Race times recordDists

parseInput :: Text -> [Race]
parseInput t =
  case parse pInput "" t of
    Left err -> error $ errorBundlePretty err
    Right res -> res

-- * Main

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  let races1 = parseInput input
  print $ product $ map getWaysToBeat races1
  print $ product $ map getWaysToBeat' races1
  let races2 = parseInput . T.filter (not . isSpace) $ input
  print $ product $ map getWaysToBeat races2
  print $ product $ map getWaysToBeat' races2
