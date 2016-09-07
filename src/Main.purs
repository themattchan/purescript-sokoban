module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Array as A

data Board = Board
  { width  :: Int
  , height :: Int
  , board  :: A.Array (A.Array Space)
  }

data Space
  = Wall
  | Box
  | Goal Bool
  | Empty
  deriving (Show, Eq)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
