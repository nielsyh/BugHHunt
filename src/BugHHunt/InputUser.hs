module BugHHunt.InputUser where

import Graphics.Gloss.Interface.Pure.Game
import Data.List
import BugHHunt.GameState
import BugHHunt.Data
import BugHHunt.Objects.Player

-- default game events are handled by:
events :: Event -> BHGameState -> BHGameState
events (EventKey (SpecialKey KeySpace) Up _ _) game = updateScore game
events (EventKey (SpecialKey k) s _ _) game = updateGameState (Just (updatePlayerMoves k s (player game))) Nothing game
events (EventKey (Char 'p') Up _ _) game = togglePaused game
events (EventKey (Char 's') Up _ _) game = game
events _ g = g

-- start screen events are handled by:
startScreenEvents :: Event -> BHGameState -> BHGameState
startScreenEvents (EventKey (Char c) Up _ _) g = g { name = name g ++ [c] }
startScreenEvents (EventKey (SpecialKey KeyBackspace) Up _ _) g = backspaceName g
startScreenEvents (EventKey (SpecialKey KeyDelete) Up _ _) g = backspaceName g
startScreenEvents (EventKey (MouseButton _) Up _ xy) g = pressedStartButton g xy
startScreenEvents _ g = g

-- remove last character from the name input
backspaceName :: BHGameState -> BHGameState
backspaceName g = g { name = if not (null (name g))
                             then init (name g)
                             else name g }

-- check if the user has pressed the start button
pressedStartButton :: BHGameState -> (Float, Float) -> BHGameState
pressedStartButton g (x, y) =
  if x >= (-65) && x <= 100 && y >= (-85) && y <= (-30) && not (null (name g))
    then g {startup = False}
    else g