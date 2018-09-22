module Types where

import Prelude

-- board :: grid of booleans, T if wall F o/w
-- box :: Coord
-- player :: Coord
-- goal :: Coord

type Board = { board  :: Array (Array Boolean) }

type Coord = { x :: Int , y :: Int }

type State =
  { board :: Board
  , player :: Coord
  , box :: Coord
  , goal :: Coord
  }

data Cell
  = Wall
  | Empty
--  deriving (Show, Eq)

data Move
  = Up
  | Down
  | Left
  | Right
--  deriving (Show, Eq)
