{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.List (tails)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

data Coord = Coord {row :: !Int, col :: !Int} deriving (Show, Eq, Ord)

expandGalaxy :: Int -> Set Coord -> Set Coord
expandGalaxy scale cs =
  let coordRows = S.map row cs
      coordCols = S.map col cs
      allRows = S.fromList [0 .. S.findMax coordRows]
      allCols = S.fromList [0 .. S.findMax coordCols]
      unusedRows = S.difference allRows coordRows
      unusedCols = S.difference allCols coordCols
      updateCoord :: Coord -> Coord
      updateCoord (Coord r c) =
        let emptyRowsBefore = S.size $ S.filter (< r) unusedRows
            emptyColsBefore = S.size $ S.filter (< c) unusedCols
         in Coord (r + scale * emptyRowsBefore) (c + scale * emptyColsBefore)
   in S.map updateCoord cs

manhattan :: Coord -> Coord -> Int
manhattan (Coord r1 c1) (Coord r2 c2) = abs (r1 - r2) + abs (c1 - c2)

-- * Parsing

type Parser = Parsec Void Text

pEmpty :: Parser ()
pEmpty = void (char '.' <|> newline)

-- | returns zero-indexed coordinates
pCoord :: Parser Coord
pCoord = do
  _ <- char '#'
  pos <- getSourcePos
  pure $ Coord (unPos (sourceLine pos) - 1) (unPos (sourceColumn pos) - 2)

pInput :: Parser (Set Coord)
pInput = do
  _ <- many pEmpty
  cs <- some (pCoord <* many pEmpty)
  eof
  pure $ S.fromList cs

parseInput :: Text -> Set Coord
parseInput t =
  case parse pInput "" t of
    Left e -> error $ errorBundlePretty e
    Right r -> r

-- * Main

generateUniquePairs :: Set a -> [(a, a)]
generateUniquePairs s =
  let xs = S.toList s
   in do
        -- Ooh list monad! Don't get to use this often.
        (x, otherYs) <- zip xs (tail $ tails xs)
        y <- otherYs
        pure (x, y)

sumAllManhattan :: Set Coord -> Int
sumAllManhattan cs = sum $ map (uncurry manhattan) $ generateUniquePairs cs

main :: IO ()
main = do
  galaxy <- parseInput <$> T.readFile "input.txt"
  print $ sumAllManhattan $ expandGalaxy 1 galaxy
  print $ sumAllManhattan $ expandGalaxy 999999 galaxy
