module BugHHunt.Graphics where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy
import Data.Maybe
import Data.List
import BugHHunt.Objects.HighScore

-- sprites datatype holds all loaded images
data Sprites = Sprites {
  backgroundSprite :: Picture,
  playerSprite :: Picture,
  bugSprite :: Picture,
  backgroundText :: Picture
}

-- load the images into a sprite datastructure using Juicy
loadSprites :: IO Sprites
loadSprites = do
    backgroundTexture <- loadJuicyPNG "res/background.png"
    playerTexture <- loadJuicyPNG "res/player.png"
    bugTexture <- loadJuicyPNG "res/bug.png"
    backgroundText <- loadJuicyPNG "res/background_text.png"

    return Sprites {
      backgroundSprite = fromJust backgroundTexture,
      playerSprite = fromJust playerTexture,
      bugSprite = fromJust bugTexture,
      backgroundText = fromJust backgroundText
    }

-- render the background text
renderBackground :: Sprites -> Picture
renderBackground = backgroundText

-- render the score
renderScore :: Int -> Picture
renderScore g = Translate (-(half width - 35)) (half height - 50) $ Scale 0.2 0.2 $ Color white $ Text ("Score " ++ show g)

-- render the level
renderLevel :: Int -> Picture
renderLevel g = Translate (half width - 100) (half height - 50) $ Scale 0.2 0.2 $ Color white $ Text ("Level " ++ show g)

-- render the lives
renderLives :: Int -> Picture
renderLives g = Translate (-50) (half height - 30) $ Scale 0.2 0.2 $ Color white $ Text ("Lives " ++ show g)

-- render the pause button and text
renderPause :: Bool -> Picture
renderPause g | g = Translate (-100) (-50) $ Scale 1 1 $ Color white $ Text "Paused!"
              | otherwise = Translate (-50) (-(half height - 10)) $ Scale 0.1 0.1 $ Color white $ Text "Pres p for pause"

-- render the game-over text
renderGameOver = Translate (-200) (-50) $ Scale 0.6 1 $ Color white $ Text "Game Over!"

-- render the cracks in the screen
renderBroken :: Bool -> Picture 
renderBroken b | b = line [
  (-50, half height),
  ( 0, half height-100),
  (-100,  - 100),
  (100, -(half height)),
  (200, 0),
  (0, 100),
  (half width - 100, half height - 100),
  (100, half height)
  ]
              | otherwise = blank

-- render enter name text
renderEnterName :: Picture -- render "enter your name" to startscreen
renderEnterName = Translate (-200) (80) $ Scale 0.4 0.4 $ Color white $ Text "Enter your name:"

-- render enter name input
renderNameInput :: String -> Picture -- render name input to startscreen
renderNameInput n = Translate (-50) 0 $ Scale 0.4 0.4 $ Color white $ Text n

-- render start button
renderStartButton :: Picture -- renders the start button to startscreen
renderStartButton = pictures [Translate (-60) (-80) $ Scale 0.4 0.4 $ Color white $ Text "START",
                    line [(-65, -85), (100, -85), (100, -30), (-65, -30), (-65, -85)] ]

-- render highscores as a list under each other
renderHighScores :: [HighScore] -> Picture -- Highscore picture function, renders to startscreen
renderHighScores hs = pictures [Translate ((-half width) + 20) 100 $ Scale 0.2 0.2 $ Color white $ Text "Highscores:", renderHighScores' hs 60]

renderHighScores' :: [HighScore] -> Float -> Picture -- Highscore picture help function
renderHighScores' [] _  = blank
renderHighScores' (x:xs) y = pictures [ renderHighScores' xs (y - 40),  Translate ((-half width) + 20) y $ Scale 0.2 0.2 $ Color white $ Text (show x)]

-- create the display window the game is played in
window :: Display -- Window for animation
window = InWindow "Bug HHunt" (floatToInt width, floatToInt height) (0, 0)

-- set background color
background :: Color -- I see blue..
background = blue

fps :: Int -- frames per second
fps = 30

height :: Float -- y resolution
height = 600

width :: Float -- x resolution
width = 1200

half :: Float -> Float -- float divided by two
half x = x / 2

floatToInt :: Float -> Int -- round, float to int
floatToInt = round