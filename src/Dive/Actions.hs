module Dive.Actions where

import Prelude hiding ((.), id)

import Control.Category
import Data.Label
import qualified Data.Map as Map

import Dive.GameState

data Command = UICommand UIAction
             | GameCommand Action
             deriving (Show, Eq)

data UIAction = Exit deriving (Show, Eq)

data Action = Move Dir | Rest deriving (Show, Eq)

data Dir = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq)

data ActionError = DirectionBlocked deriving (Show, Eq)

-- * Performing game actions.

performAction :: Action -> GameState -> Either ActionError GameState
performAction Rest       gs = Right gs
performAction (Move dir) gs =
  let newPos  = stepPos dir $ get (position . player) gs
      newTile = Map.lookup newPos (get tiles gs)
  in if newTile == Just Floor
     then Right $ set (position . player) newPos gs
     else Left DirectionBlocked

stepPos :: Dir -> Pos -> Pos
stepPos N  (x, y) = (x    , y - 1)
stepPos NE (x, y) = (x + 1, y - 1)
stepPos E  (x, y) = (x + 1, y)
stepPos SE (x, y) = (x + 1, y + 1)
stepPos S  (x, y) = (x    , y + 1)
stepPos SW (x, y) = (x - 1, y + 1)
stepPos W  (x, y) = (x - 1, y)
stepPos NW (x, y) = (x - 1, y - 1)
