module Parser where

import Prelude
import Data.Bifunctor
import Data.Tuple
import Data.List hiding (many)

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Combinators (sepBy, manyTill, choice)

import Types

-- Wall                     #   0x23
-- Player                   @   0x40
-- Player on goal square    +   0x2b
-- Box                      $   0x24
-- Box on goal square       *   0x2a
-- Goal square              .   0x2e
-- Floor                (Space) 0x20

charIs :: forall a. Char -> a -> Parser String a
charIs c a = char c $> a

newline = char '\n'

wall :: Parser String Cell
wall = charIs '#' Wall

playerNonGoal :: Parser String Cell
playerNonGoal = charIs '@' Player

playerGoal :: Parser String Cell
playerGoal = charIs '+' GoalPlayer

boxNonGoal :: Parser String Cell
boxNonGoal = charIs '$' Box

boxGoal :: Parser String Cell
boxGoal = charIs '*' GoalBox

goal :: Parser String Cell
goal = charIs '.' GoalEmpty

floor :: Parser String Cell
floor = charIs ' ' Empty

parseCell :: Parser String Cell
parseCell = choice [ wall, playerNonGoal, playerGoal
                   , boxNonGoal, boxGoal, goal, floor ]

parseRow :: Parser String (List Cell)
parseRow = manyTill parseCell newline

--parseBoard :: Parser String State
parseBoard = do
  board <- map transformAnnot parseBoard1
  pure board
  where
    transformAnnot :: (List (Tuple (List (Tuple Cell Int)) Int))
                   -> (List (List (Tuple Cell (Tuple Int Int))))
    transformAnnot = map (\(Tuple row ri) ->
                           map (rmap (Tuple ri)) row)

    parseBoard1 :: Parser String (List (Tuple (List (Tuple Cell Int)) Int))
    parseBoard1 = parseAnnot (sepBy (parseAnnot parseRow) newline)

    parseAnnot :: forall a b. Parser a (List b) -> Parser a (List (Tuple b Int))
    parseAnnot p = do
      r <- p
      let n = length r
      pure $ zip r (range 0 (n-1))
