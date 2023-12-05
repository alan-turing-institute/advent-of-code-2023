{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

parseDigit :: Bool -> Text -> Maybe Int
parseDigit acceptWords t
  | "0" `T.isPrefixOf` t = Just 0
  | "1" `T.isPrefixOf` t = Just 1
  | "2" `T.isPrefixOf` t = Just 2
  | "3" `T.isPrefixOf` t = Just 3
  | "4" `T.isPrefixOf` t = Just 4
  | "5" `T.isPrefixOf` t = Just 5
  | "6" `T.isPrefixOf` t = Just 6
  | "7" `T.isPrefixOf` t = Just 7
  | "8" `T.isPrefixOf` t = Just 8
  | "9" `T.isPrefixOf` t = Just 9
  | acceptWords && "zero" `T.isPrefixOf` t = Just 0
  | acceptWords && "one" `T.isPrefixOf` t = Just 1
  | acceptWords && "two" `T.isPrefixOf` t = Just 2
  | acceptWords && "three" `T.isPrefixOf` t = Just 3
  | acceptWords && "four" `T.isPrefixOf` t = Just 4
  | acceptWords && "five" `T.isPrefixOf` t = Just 5
  | acceptWords && "six" `T.isPrefixOf` t = Just 6
  | acceptWords && "seven" `T.isPrefixOf` t = Just 7
  | acceptWords && "eight" `T.isPrefixOf` t = Just 8
  | acceptWords && "nine" `T.isPrefixOf` t = Just 9
  | otherwise = Nothing

data ParseDirection = FromStart | FromEnd deriving (Eq)

getDigit :: ParseDirection -> Bool -> Text -> Maybe Int
getDigit direction acceptWords t =
  let n = T.length t
      f = if direction == FromStart then T.drop else T.takeEnd
   in foldr (<|>) Nothing $ map (\i -> parseDigit acceptWords (f i t)) [0 .. n]

parseLine :: Bool -> Text -> Int
parseLine acceptWords t =
  case (getDigit FromStart acceptWords t, getDigit FromEnd acceptWords t) of
    (Just x, Just y) -> x * 10 + y
    (m, n) -> error $ "Failed to parse line: `" <> T.unpack t <> "`. Got: " <> show (m, n)

part1 :: [Text] -> Int
part1 = sum . map (parseLine False)

part2 :: [Text] -> Int
part2 = sum . map (parseLine True)

main :: IO ()
main = do
  inputLines <- T.lines <$> T.readFile "input.txt"
  print $ part1 inputLines
  print $ part2 inputLines
