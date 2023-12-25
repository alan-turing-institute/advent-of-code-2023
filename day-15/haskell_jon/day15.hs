{-# LANGUAGE OverloadedStrings #-}

import Data.Char (ord)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Monoid (Sum (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

data Instruction
  = Add !Text !Int -- label and focal length
  | Remove !Text -- label
  deriving (Eq, Ord, Show)

data Lens = Lens {label :: !Text, focalLength :: !Int}
  deriving (Eq, Ord, Show)

updateWithInstruction :: Instruction -> [Lens] -> [Lens]
updateWithInstruction (Add lb' fl') [] = [Lens lb' fl']
updateWithInstruction ins@(Add lb' fl') (l : ls) =
  if lb' == label l
    then Lens lb' fl' : ls
    else l : updateWithInstruction ins ls
updateWithInstruction (Remove lb') [] = []
updateWithInstruction ins@(Remove lb') (l : ls) =
  if lb' == label l
    then ls
    else l : updateWithInstruction ins ls

runInstruction :: IntMap [Lens] -> Instruction -> IntMap [Lens]
runInstruction m inst =
  let label = case inst of
        Add lb _ -> lb
        Remove lb -> lb
      k = hash label
   in IM.adjust (updateWithInstruction inst) k m

initialMap :: IntMap [Lens]
initialMap = IM.fromList [(i, []) | i <- [0 .. 255]]

hash :: Text -> Int
hash = T.foldl' (\acc c -> ((acc + ord c) * 17) `mod` 256) 0

focusingPower :: Int -> [Lens] -> Sum Int
focusingPower boxNumber ls =
  Sum $
    (boxNumber + 1) * sum (zipWith (*) (map focalLength ls) [1 ..])

part2 :: [Instruction] -> Int
part2 ins =
  let finalMap = foldl' runInstruction initialMap ins
   in getSum $ IM.foldMapWithKey focusingPower finalMap

-- * Parsing (part 2 only)

type Parser = Parsec Void Text

parseInstruction :: Parser Instruction
parseInstruction = do
  label <- T.pack <$> some lowerChar
  isAdd <- (True <$ char '=') <|> (False <$ char '-')
  focalLength <- if isAdd then L.decimal else pure 0
  pure $ if isAdd then Add label focalLength else Remove label

parseInputP2 :: Parser [Instruction]
parseInputP2 = do
  ins <- parseInstruction
  ins' <- many (char ',' *> parseInstruction)
  pure $ ins : ins'

parseWith :: Parser a -> Text -> a
parseWith p t =
  case parse p "" t of
    Left e -> error $ errorBundlePretty e
    Right r -> r

-- * Main

part1 :: [Text] -> Int
part1 = sum . map hash

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  let part1Steps = T.splitOn "," . T.strip $ input
  print $ part1 part1Steps
  let part2Steps = parseWith parseInputP2 input
  print $ part2 part2Steps
