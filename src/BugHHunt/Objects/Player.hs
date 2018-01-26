module BugHHunt.Objects.Player where

import Data.Maybe

import BugHHunt.Data
import BugHHunt.Graphics

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- player datatype
data Player = Player {
  playerResource :: Resource,
  playerCoords :: Point,
  playerMoves :: PlayerMoves,
  defaultSpeed :: Float
}

-- keystates of the player in which direction it should update
type PlayerMoves = (KeyState, KeyState, KeyState, KeyState)

-- initialize player
initPlayer :: Sprites -> Player
initPlayer s = Player {
  playerResource = (0.5, playerSprite s),
  playerCoords = (0, 0),
  playerMoves = (Up, Up, Up, Up),
  defaultSpeed = 300
}

-- reset player to the center
resetPlayer :: Player -> Player
resetPlayer p = p {
                   playerCoords = (0, 0),
                   defaultSpeed = 300
                   }

-- move player between game bounds
movePlayer :: Point -> Player -> Player
movePlayer c p  | pointX c > half width 
              || pointX c < (-(half width))
              || pointY c > half height 
              || pointY c < (-(half height)) = p 
                | otherwise =  p {playerCoords = c}

-- update the keystates of the player accordingly to the pressed button
updatePlayerMoves :: SpecialKey -> KeyState -> Player -> Player
updatePlayerMoves k s p = p {playerMoves = updatePlayerMoves' k s (playerMoves p)}

updatePlayerMoves' :: SpecialKey -> KeyState -> PlayerMoves -> PlayerMoves
updatePlayerMoves' KeyUp s (_, down, left, right) = (s, down, left, right)
updatePlayerMoves' KeyDown s (up, _, left, right) = (up, s, left, right)
updatePlayerMoves' KeyLeft s (up, down, _, right) = (up, down, s, right)
updatePlayerMoves' KeyRight s (up, down, left, _) = (up, down, left, s)
updatePlayerMoves' _ _ m = m

-- draw the player
instance Drawable Player where
  draw b = translate (pointX (playerCoords b)) (pointY (playerCoords b)) (resource (playerResource b))

-- move the player
instance Moveable Player where
  mTranslate (x, y) p = movePlayer (pointX (playerCoords p ) + x, pointY (playerCoords p ) + y) p

-- update the player
instance Updateable Player where
  uUpdate s p = mTranslate (x (playerMoves p) * s, y (playerMoves p) * s) p
    where x (_, _, Up, Down) = speed
          x (_, _, Down, Up) = -speed
          x _ = 0
          y (Down, Up, _, _) = speed
          y (Up, Down, _, _) = -speed
          y _ = 0
          speed = defaultSpeed p