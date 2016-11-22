module Types where

--import Prelude


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
--  deriving (Show, Eq)

data Move
  = Up
  | Down
  | Left
  | Right
--  deriving (Show, Eq)
