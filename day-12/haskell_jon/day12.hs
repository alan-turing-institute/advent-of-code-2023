{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate, tails)
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

data Condition = Operational | Damaged | Unknown deriving (Eq, Ord, Enum, Bounded)

instance Show Condition where
  show Operational = "."
  show Damaged = "#"
  show Unknown = "?"

data DamagedOrUnknown = Damaged' | Unknown' deriving (Eq, Ord)

instance Show DamagedOrUnknown where
  show Damaged' = "#"
  show Unknown' = "?"

data DamagedOrOperational = Damaged'' | Operational'' deriving (Eq, Ord)

instance Show DamagedOrOperational where
  show Damaged'' = "#"
  show Operational'' = "."

condToDamagedOrUnknown :: Condition -> DamagedOrUnknown
condToDamagedOrUnknown Operational = error "urk"
condToDamagedOrUnknown Damaged = Damaged'
condToDamagedOrUnknown Unknown = Unknown'

data ConditionRecord = ConditionRecord
  { conditions :: ![Condition],
    damagedConfig :: ![Int]
  }
  deriving (Eq, Ord, Show)

countKern :: (ConditionRecord -> Int) -> ConditionRecord -> Int
countKern _ (ConditionRecord [] []) = 1
countKern _ (ConditionRecord [] _) = 0
countKern _ (ConditionRecord conds []) = if Damaged `elem` conds then 0 else 1
countKern recurse (ConditionRecord conds cfg@(firstDamagedLen : restCfg)) =
  case conds of
    Operational : rem -> recurse (ConditionRecord rem cfg)
    Unknown : rem ->
      recurse (ConditionRecord rem cfg)
        + calcAfterDamaged (ConditionRecord rem cfg)
    Damaged : rem -> calcAfterDamaged (ConditionRecord rem cfg)
  where
    -- The reason why we have a separate calcAfterDamaged function (instead of
    -- just recursing) is to ensure that any argument passed to `recurse` is a
    -- true suffix of the original condition record. If we don't do this, then
    -- the dynamic programming table cannot be filled in correctly.
    calcAfterDamaged (ConditionRecord rem' _) =
      let (firstNm1, rest) = splitAt (firstDamagedLen - 1) rem'
       in if Operational `elem` firstNm1 || length firstNm1 < firstDamagedLen - 1
            then 0
            else case rest of
              (Damaged : _) -> 0
              (Unknown : rest') -> recurse (ConditionRecord rest' restCfg)
              _anythingElse -> recurse (ConditionRecord _anythingElse restCfg)

-- | Dynamic programming (!!!)
tabulateWith :: ConditionRecord -> ((ConditionRecord -> Int) -> ConditionRecord -> Int) -> ConditionRecord -> Int
tabulateWith (ConditionRecord fullConds fullCfg) kern = fun
  where
    fun c = case M.lookup c table of
      Just r -> r
      Nothing -> error $ show c
    table =
      M.fromList $
        map (\c -> (c, kern fun c)) $
          [ ConditionRecord conds cfg | conds <- tails fullConds, cfg <- tails fullCfg
          ]

countN :: ConditionRecord -> Int
countN c = tabulateWith c countKern c

-- * Parsing

type Parser = Parsec Void Text

pCondition :: Parser Condition
pCondition =
  choice
    [ Operational <$ char '.',
      Damaged <$ char '#',
      Unknown <$ char '?'
    ]

pLine :: Parser ConditionRecord
pLine = do
  conds <- some pCondition
  _ <- char ' '
  arr <- some (L.decimal <* optional (char ','))
  eof
  return $ ConditionRecord conds arr

parseWith :: Parser a -> Text -> a
parseWith p t =
  case parse p "" t of
    Left e -> error $ errorBundlePretty e
    Right r -> r

-- * Main

part1 :: [ConditionRecord] -> Int
part1 records = sum $ map countN records

part2 :: [ConditionRecord] -> Int
part2 records = sum $ map (countN . expand) records
  where
    expand :: ConditionRecord -> ConditionRecord
    expand (ConditionRecord conds cfg) =
      ConditionRecord (intercalate [Unknown] (replicate 5 conds)) (concat (replicate 5 cfg))

main :: IO ()
main = do
  records <- map (parseWith pLine) . T.lines <$> T.readFile "input.txt"
  print $ part1 records
  print $ part2 records
