{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) xs

forwardExtrapolate :: [Int] -> Int
forwardExtrapolate [] = error "empty list"
forwardExtrapolate l@(x : xs) =
  if all (== x) xs
    then x
    else last l + forwardExtrapolate (diffs l)

backwardExtrapolate :: [Int] -> Int
backwardExtrapolate [] = error "empty list"
backwardExtrapolate l@(x : xs) =
  if all (== x) xs
    then x
    else x - backwardExtrapolate (diffs l)

-- * Parsing

type Parser = Parsec Void Text

lx :: Parser a -> Parser a
lx = L.lexeme (L.space space1 empty empty)

pSignedDecimal :: Parser Int
pSignedDecimal = do
  neg <- optional $ char '-'
  dec <- L.decimal
  pure $ case neg of
    Nothing -> dec
    Just _ -> -dec

pList :: Parser [Int]
pList = many (lx pSignedDecimal)

parseLine :: Text -> [Int]
parseLine t =
  case parse pList "" t of
    Left e -> error $ errorBundlePretty e
    Right r -> r

-- * Main

main :: IO ()
main = do
  seqs <- map parseLine . T.lines <$> T.readFile "input.txt"
  print $ sum $ map forwardExtrapolate seqs
  print $ sum $ map backwardExtrapolate seqs
