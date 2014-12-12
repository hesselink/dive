{-# LANGUAGE TemplateHaskell #-}
module Dive.GameState where

import Data.Label
import Data.Map
import qualified Data.Map as Map

type Pos = (Int, Int)
data Tile = Floor | Wall deriving (Show, Eq)

fclabels [d|
  data Player = Player
    { position :: Pos
    , health   :: Integer
    } deriving (Show, Eq)

  data GameState = GameState
    { tiles  :: Map Pos Tile
    , player :: Player
    } deriving (Show, Eq)
  |]

initialGameState :: GameState
initialGameState = GameState initialMap initialPlayer

initialMap :: Map Pos Tile
initialMap = Map.fromList
  [ ((x, y), if abs x == 5 || abs y == 5 then Wall else Floor)
  | x <- [-5 .. 5], y <- [-5 .. 5]
  ]

initialPlayer :: Player
initialPlayer = Player (0, 0) 100
