module Types where

import Prelude
import Matrix (Matrix)
import Data.Newtype
import Data.List.NonEmpty


type Board = Matrix Cell -- could just store this with a range tree

-- top left is (y:0,x:0), move one to right is (y:0,x:1), one down is (y:1,x:0)
type Coord =
  { x :: Int
  , y :: Int
  }

newtype Player = Player Coord
derive instance newtypePlayer :: Newtype Player _

-- | A box is represented by a coord
newtype Box = Box Coord
derive instance newtypeBox :: Newtype Box _

type Boxes = NonEmptyList Coord

type Level =
  { board :: Board
  , player :: Player
  , boxes :: Boxes
  , levelName :: String
  }

type NamedBoard =
  { board :: Board
  , levelName :: String
  }

data Cell
  = Wall
  | Goal
  | CompletedGoal
  | Empty
instance showCell :: Show Cell where
  show Wall = "#"
  show Goal = "."
  show CompletedGoal = "*"
  show Empty = " "

--derive instance showCell :: Show Cell
derive instance eqCell :: Eq Cell

data Direction
  = Up
  | Down
  | Left
  | Right

instance showDirection :: Show Direction where
  show Up = "Up"
  show Down = "Down"
  show Left = "Left"
  show Right = "Right"
--derive instance showDirection :: Show Direction
derive instance eqDirection :: Eq Direction

newtype Orientation = Orientation Direction
derive instance newtypeOrientation :: Newtype Orientation _

newtype Move = Move Direction
derive instance newtypeMove :: Newtype Move _
