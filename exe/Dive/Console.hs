module Dive.Console where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category
import Control.Monad.Trans
import Data.Function (on)
import Data.Label
import Data.List
import Graphics.Vty (Vty)
import qualified Data.Map     as Map
import qualified Graphics.Vty as Vty

import Dive.Actions
import Dive.GameState

-- * Printing things to the console.

ppr :: MonadIO m => GameState -> Vty -> m ()
ppr gs vty = liftIO $ do
  (bx, by) <- Vty.displayBounds (Vty.outputIface vty)
  let (px, py) = get (position . player) gs
      (tlx, tly) = (px - bx `div` 2, py - by `div` 2)
      (brx, bry) = (px + bx `div` 2, py + by `div` 2)
      ts = [ (p, Map.lookup p (get tiles gs)) | x <- [tlx .. brx], y <- [tly .. bry], let p = (x, y) ]
      pic = Vty.picForLayers
        [imagePlayer (tlx, tly) (get player gs)
        , foldMap (imageMonster (tlx, tly)) (get monsters gs)
        , imageTiles ts]
  Vty.update vty pic

imagePlayer :: Pos -> Player -> Vty.Image
imagePlayer (tlx, tly) p =
  let (px, py) = get position p
  in Vty.pad (px - tlx) (py - tly) 0 0 $
       Vty.char (Vty.defAttr `Vty.withForeColor` Vty.red) '@'

imageMonster :: Pos -> Monster -> Vty.Image
imageMonster (tlx, tly) m =
  let (mx, my) = get mPosition m
  in Vty.pad (mx - tlx) (my - tly) 0 0 $
       imageMonsterSymbol (get mType m)

imageMonsterSymbol :: MonsterType -> Vty.Image
imageMonsterSymbol Rat = Vty.char (Vty.defAttr `Vty.withForeColor` Vty.yellow) 'r'

-- | Invariant: ordered by x position then y position, every position
-- in the grid is present.

imageTiles :: [(Pos, Maybe Tile)] -> Vty.Image
imageTiles = Vty.horizCat . map (imageCol . map snd) .  groupBy ((==) `on` fst . fst)

imageCol :: [Maybe Tile] -> Vty.Image
imageCol = Vty.vertCat . map imageTile

imageTile :: Maybe Tile -> Vty.Image
imageTile Nothing      = Vty.char (Vty.defAttr `Vty.withForeColor` Vty.red) ' '
imageTile (Just Floor) = Vty.char (Vty.defAttr `Vty.withForeColor` Vty.red) '.'
imageTile (Just Wall ) = Vty.char (Vty.defAttr `Vty.withForeColor` Vty.red) '#'

-- * Parsing events to commands.

eventToCmd :: Vty.Event -> Maybe Command
eventToCmd (Vty.EvKey k mods) = keyToCmd k mods
eventToCmd (Vty.EvMouseUp _x _y _b) = Nothing
eventToCmd (Vty.EvMouseDown _x _y _b _mods) = Nothing
eventToCmd (Vty.EvResize _x _y) = Nothing
eventToCmd (Vty.EvPaste _bs) = Nothing
eventToCmd Vty.EvGainedFocus = Nothing
eventToCmd Vty.EvLostFocus = Nothing

keyToCmd :: Vty.Key -> [Vty.Modifier] -> Maybe Command
keyToCmd k ms =  UICommand   <$> keyToUICmd   k ms
             <|> GameCommand <$> keyToGameCmd k ms

keyToUICmd :: Vty.Key -> [Vty.Modifier] -> Maybe UIAction
keyToUICmd Vty.KEsc        [] = Just Exit
keyToUICmd (Vty.KChar 'q') [] = Just Exit
keyToUICmd _               _  = Nothing

keyToGameCmd :: Vty.Key -> [Vty.Modifier] -> Maybe Action
keyToGameCmd (Vty.KChar 'h') [] = Just $ Move W
keyToGameCmd (Vty.KChar 'j') [] = Just $ Move S
keyToGameCmd (Vty.KChar 'k') [] = Just $ Move N
keyToGameCmd (Vty.KChar 'l') [] = Just $ Move E
keyToGameCmd (Vty.KChar 'y') [] = Just $ Move NW
keyToGameCmd (Vty.KChar 'u') [] = Just $ Move NE
keyToGameCmd (Vty.KChar 'b') [] = Just $ Move SW
keyToGameCmd (Vty.KChar 'n') [] = Just $ Move SE
keyToGameCmd (Vty.KChar '.') [] = Just $ Rest
keyToGameCmd Vty.KLeft       [] = Just $ Move W
keyToGameCmd Vty.KRight      [] = Just $ Move E
keyToGameCmd Vty.KUp         [] = Just $ Move N
keyToGameCmd Vty.KDown       [] = Just $ Move S
keyToGameCmd Vty.KUpLeft     [] = Just $ Move NW
keyToGameCmd Vty.KUpRight    [] = Just $ Move NE
keyToGameCmd Vty.KDownLeft   [] = Just $ Move SW
keyToGameCmd Vty.KDownRight  [] = Just $ Move SE
keyToGameCmd Vty.KCenter     [] = Just $ Rest
keyToGameCmd _               _  = Nothing
