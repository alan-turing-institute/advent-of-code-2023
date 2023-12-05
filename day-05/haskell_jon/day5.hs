{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- * Types


-- * Parsing

parseLine :: Text -> 
parseLine t =
  case parse pCard "" t of
    Left err -> error $ errorBundlePretty err
    Right res -> res

-- * Main

part1 :: [Text] -> Int
part1 = const 42

part2 :: [Text] -> Int
part2 = const 42

main :: IO ()
main = do
  input <- parseInput <$> T.readFile "input.txt"
  print $ part1 input
