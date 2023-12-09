{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA3)
import Control.DeepSeq
import Control.Monad (replicateM)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Types

-- Don't use Left and Right as constructors because they are already used in
-- Either.
data Instruction = L | R deriving (Show, Eq)

data NodeName = NodeName !Char !Char !Char deriving (Show, Eq, Ord, Generic, NFData)

data Node = Node {left :: !NodeName, right :: !NodeName} deriving (Show, Eq, Generic, NFData)

follow :: Map NodeName Node -> Instruction -> NodeName -> NodeName
follow graph instruction nodeName =
  case instruction of
    L -> left node
    R -> right node
  where
    node = graph M.! nodeName

-- | The 'correct' way to do it. Oh well.
stepsTo ::
  -- | Names of nodes to start from
  [NodeName] ->
  -- | Predicate to check if we have reached the end
  ([NodeName] -> Bool) ->
  -- | Instructions to follow
  [Instruction] ->
  -- | Graph of nodes
  Map NodeName Node ->
  -- | Accumulator for the number of steps taken so far. Should be 0 when
  -- calling this function.
  Int ->
  -- | The number of steps taken so far
  Int
stepsTo startNames endNamesPredicate instructions graph acc =
  if endNamesPredicate startNames
    then acc
    else
      let (thisInstruction : remainingInstructions) = instructions
          nextNames = map (follow graph thisInstruction) startNames
       in nextNames `deepseq` stepsTo nextNames endNamesPredicate remainingInstructions graph (acc + 1)

stepsToFirstZ :: NodeName -> [Instruction] -> Map NodeName Node -> Int
stepsToFirstZ startName instructions graph =
  stepsTo [startName] (all $ \(NodeName _ _ z) -> z == 'Z') instructions graph 0

-- * Parsing

type Parser = Parsec Void Text

lx :: Parser a -> Parser a
lx = L.lexeme (L.space space1 empty empty)

pInstruction :: Parser [Instruction]
pInstruction = many (L <$ char 'L' <|> R <$ char 'R')

pNodeName :: Parser NodeName
pNodeName = liftA3 NodeName alphaNumChar alphaNumChar alphaNumChar

pTextAndNode :: Parser (NodeName, Node)
pTextAndNode = do
  nodeName <- lx pNodeName
  _ <- string "= ("
  left <- lx pNodeName
  _ <- string ", "
  right <- lx pNodeName
  _ <- string ")"
  pure (nodeName, Node left right)

pInput :: Parser ([Instruction], Map NodeName Node)
pInput = do
  instructions <- lx pInstruction
  nodes <- some $ lx pTextAndNode
  pure (instructions, M.fromList nodes)

parseInput :: Text -> ([Instruction], Map NodeName Node)
parseInput t =
  case parse pInput "" t of
    Left e -> error $ errorBundlePretty e
    Right r -> r

part1 :: [Instruction] -> Map NodeName Node -> Int
part1 instructions graph =
  stepsTo [NodeName 'A' 'A' 'A'] (== [NodeName 'Z' 'Z' 'Z']) (cycle instructions) graph 0

-- | Too slow
part2 :: [Instruction] -> Map NodeName Node -> Int
part2 instructions graph =
  let startNodeNames = filter (\(NodeName _ _ a) -> a == 'A') (M.keys graph)
      endPredicate = all (\(NodeName _ _ z) -> z == 'Z')
   in stepsTo startNodeNames endPredicate (cycle instructions) graph 0

-- | Exploits periodicity of graph traversal
part2' :: [Instruction] -> Map NodeName Node -> Int
part2' instructions graph =
  let startNodeNames = filter (\(NodeName _ _ a) -> a == 'A') (M.keys graph)
   in foldl' lcm 1 $ map (\n -> stepsToFirstZ n (cycle instructions) graph) startNodeNames

-- * Main

main :: IO ()
main = do
  (instructions, graph) <- parseInput <$> T.readFile "input.txt"
  print $ part1 instructions graph
  print $ part2' instructions graph
