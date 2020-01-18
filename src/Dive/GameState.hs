{-# LANGUAGE TemplateHaskell #-}
module Dive.GameState where

import Data.Label
import Data.Map
import qualified Data.Map as Map

type Pos = (Int, Int)
data Tile = Floor | Wall deriving (Show, Eq)

data MonsterType = Rat
  deriving (Show, Eq)

fclabels [d|
  data Player = Player
    { position :: Pos
    , health   :: Integer
    } deriving (Show, Eq)

  data Monster = Monster
    { mPosition :: Pos
    , mHealth   :: Integer
    , mType     :: MonsterType
    } deriving (Show, Eq)

  data GameState = GameState
    { tiles    :: Map Pos Tile
    , player   :: Player
    , monsters :: [Monster]
    } deriving (Show, Eq)
  |]

initialGameState :: GameState
initialGameState = GameState initialMap initialPlayer [initialMonster]

initialMap :: Map Pos Tile
initialMap = Map.fromList
  [ ((x, y), if abs x == 5 || abs y == 5 then Wall else Floor)
  | x <- [-5 .. 5], y <- [-5 .. 5]
  ]

initialPlayer :: Player
initialPlayer = Player (0, 0) 100

initialMonster :: Monster
initialMonster = Monster (2,3) 10 Rat
