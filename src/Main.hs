module Main(main) where

import System.Random (StdGen, newStdGen)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import BugHHunt.Data
import BugHHunt.Graphics
import BugHHunt.GameState
import BugHHunt.InputUser
import BugHHunt.Objects.HighScore

-- load main game
main :: IO ()
main = do
  ioSprites <- loadSprites
  ioRng <- newStdGen
  ioHighScore <- readFile "res/highscores.json"
  playIO window background fps (initialState ioRng ioSprites (readHighScores ioHighScore)) renderIO eventsIO update

-- unwrap IO monad for rendering
renderIO :: BHGameState -> IO Picture
renderIO g = return (render g)

-- unwrap IO monad for events
eventsIO :: Event -> BHGameState -> IO BHGameState
eventsIO e g | startup g = return (startScreenEvents e g)
             | otherwise = return (events e g)