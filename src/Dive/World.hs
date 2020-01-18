module Dive.World where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Monad
import Control.Monad.State (StateT, liftIO)
import Data.Label (get, set)
import Data.Label.Monadic (gets, modify)
import Data.List (find)
import Test.QuickCheck (arbitrary, generate)
import qualified Control.Monad.State as State
import qualified Data.Map as Map

import Dive.Actions (Action (Move, Rest), stepPos)
import Dive.GameState

worldTick :: StateT GameState IO ()
worldTick = do
  ms <- gets monsters
  forM_ ms monsterTick

monsterTick :: Monster -> StateT GameState IO ()
monsterTick m = do
  let pos = get mPosition m
  act <- liftIO $ generate arbitrary
  performAction m act

performAction :: Monster -> Action -> StateT GameState IO ()
performAction m a =
  case a of
    Rest -> return ()
    (Move d) -> do
      gs <- State.get
      let newPos = stepPos d (get mPosition m)
          monsterAtPos = find ((== newPos) . get mPosition) (get monsters gs)
          playerAtPos = get (position . player) gs == get mPosition m
          newTile = Map.lookup newPos (get tiles gs)
      case (monsterAtPos, playerAtPos, newTile) of
           (Just m, _, _) -> return () -- Monsters don't fight each other
           (_, True, _) -> return () -- TODO fight player
           (_, _, Just Floor) -> modify monsters (map (\m' -> if get mPosition m' == get mPosition m then set mPosition newPos m' else m'))
           _ -> return ()

