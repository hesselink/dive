module Dive.Actions where

import Prelude hiding ((.), id)

import Control.Category
import Data.Label
import Data.List (find)
import Test.QuickCheck (Arbitrary (arbitrary), frequency, elements)
import qualified Data.Map as Map

import Dive.GameState

data Command = UICommand UIAction
             | GameCommand Action
             deriving (Show, Eq)

data UIAction = Exit deriving (Show, Eq)

data Action = Move Dir | Rest deriving (Show, Eq)

data Dir = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq)

data ActionError = DirectionBlocked deriving (Show, Eq)

instance Arbitrary Action where
  arbitrary = frequency [(1, pure Rest), (8, Move <$> arbitrary)]

instance Arbitrary Dir where
  arbitrary = elements [N, NE, E, SE, S, SW, W, NW]

-- * Performing game actions.

performAction :: Action -> GameState -> Either ActionError GameState
performAction Rest       gs = Right gs
performAction (Move dir) gs =
  let newPos  = stepPos dir $ get (position . player) gs
      monsterAtPos = find ((== newPos) . get mPosition) (get monsters gs)
      newTile = Map.lookup newPos (get tiles gs)
  in case (monsterAtPos, newTile) of
       (Just m, _) -> Right $ fightMonster m gs
       (_, Just Floor) -> Right $ set (position . player) newPos gs
       _ -> Left DirectionBlocked

stepPos :: Dir -> Pos -> Pos
stepPos N  (x, y) = (x    , y - 1)
stepPos NE (x, y) = (x + 1, y - 1)
stepPos E  (x, y) = (x + 1, y)
stepPos SE (x, y) = (x + 1, y + 1)
stepPos S  (x, y) = (x    , y + 1)
stepPos SW (x, y) = (x - 1, y + 1)
stepPos W  (x, y) = (x - 1, y)
stepPos NW (x, y) = (x - 1, y - 1)

fightMonster :: Monster -> GameState -> GameState
fightMonster m gs =
  let damage = 1
      newMonster = modify mHealth (subtract damage) m
  in if get mHealth newMonster <= 0
     then modify monsters (filter ((/= get mPosition m) . get mPosition)) gs
     else modify monsters (map (\m' -> if get mPosition m == get mPosition m' then newMonster else m')) gs
