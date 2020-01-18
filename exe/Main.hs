module Main where

import Control.Exception
import Control.Monad.State
import Graphics.Vty (Vty, mkVty)
import qualified Graphics.Vty as Vty

import Dive.Actions (Command (..), UIAction (..), performAction)
import Dive.Console (eventToCmd, ppr)
import Dive.GameState (GameState, initialGameState)
import Dive.World (worldTick)

main :: IO ()
main = do
  let gs = initialGameState
  vtyCfg <- Vty.standardIOConfig
  bracket (mkVty vtyCfg)
          Vty.shutdown
          (gameLoop gs)

gameLoop :: GameState -> Vty -> IO ()
gameLoop initGs vty = evalStateT gameLoop' initGs
  where
    gameLoop' = do
      gs <- get
      ppr gs vty
      e <- liftIO $ Vty.nextEvent vty
      exit <- case eventToCmd e of
        Just (UICommand   Exit) -> return True
        Just (GameCommand cmd ) ->
          False <$ case performAction cmd gs of
            Left _   -> return () -- TODO: print message
            Right gs' -> do
              put gs'
              worldTick
        Nothing -> return False
      unless exit gameLoop'
