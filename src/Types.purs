module Types where

import Prelude

type Board =
  { width  :: Int
  , height :: Int
  , board  :: Array (Array Cell)
  }

type Player =
  { x :: Int
  , y :: Int
  }

type State =
  { board :: Board
  , player :: Player
  }

data Cell
  = Wall
  | Box
  | Player
  | GoalBox
  | GoalPlayer
  | GoalEmpty
  | Goal Boolean
  | Empty

derive instance showCell :: Show Cell
derive instance eqCell :: Eq Cell

data Move
  = Up
  | Down
  | Left
  | Right

derive instance showMove :: Show Move
derive instance eqMove :: Eq Move
