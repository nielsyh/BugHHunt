module BugHHunt.GameState where

import Data.Maybe
import Data.Char
import Data.List

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import BugHHunt.Graphics
import BugHHunt.Data

import BugHHunt.Objects.Player
import BugHHunt.Objects.Bug
import BugHHunt.Objects.HighScore
import BugHHunt.Objects.Bullet

import System.Random
import System.Environment

import Control.Arrow
import Data.Angle

--gamesate definition
data BHGameState = BHGameState {
-- globals
  sprites   :: Sprites,
  rng       :: StdGen,
-- objects
  player    :: Player,
  bugs      :: [Bug],
  bullets   :: [Bullet],
-- game states
  broken :: Bool,
  level :: Int,
  score :: Int,
  lives :: Int,
  paused :: Bool,
  end :: Bool,
  startup :: Bool,
  highScores :: [HighScore],
  name :: String
}

--startstate for a gamestate
initialState :: StdGen -> Sprites -> [HighScore] ->  BHGameState
initialState g s h  = BHGameState {
  sprites = s,
  rng = g1,
  player = initPlayer s,
  bugs   = bg,
  bullets = [],
  level = 1,
  score = 0,
  lives = 10,
  paused = False,
  highScores = h,
  end = False,
  broken = False,
  startup = True,
  name = ""
}
  where (bg, g1) = initRandomBugs s 1 g 1

  -- Render function for gamestate
render :: BHGameState -> Picture
render game | startup game = pictures [renderEnterName, renderNameInput (name game), renderStartButton, renderHighScores (highScores game)]
            | endOfGame game = renderGameOver
            | otherwise = pictures [renderBackground (sprites game),
                        draw (bugs game), draw (bullets game), draw (player game),
                        renderScore (score game), renderLevel (level game),
                        renderLives (lives game), renderPause (paused game),
                        renderBroken (broken game)]

--update the gamestate, expl: movebugs, check if they are dead or out of the screen.
update :: Float -> BHGameState -> IO BHGameState
update s g | paused g = return g
           | endOfGame g = do saveHighScores (highScores (updateHighScores g))
                              return (updateHighScores g) {paused = True, end = True}
           | startup g = return g
           | otherwise = return game
           where game = updated g {
                              player = uUpdate s (player (updated g)),
                              bugs = uUpdate s (bugs (updated g)),
                              bullets = filter (\b -> bulletAnimFrame b > 0) (uUpdate s (bullets (updated g)))
                        }
                        where updated = updateBugAngles >>> checkFledBugs >>> checkZeroBugs >>> deadBugs

--depending on the bug location calls update angle for all bugs
updateBugAngles :: BHGameState -> BHGameState
updateBugAngles g = g { bugs = map bug' (bugs g) }
  where bug' b | pointX (bugCoords b) > w     = b {bugAngle = Degrees 90 }
               | pointX (bugCoords b) < (-w)  = b {bugAngle = Degrees 270}
               | pointY (bugCoords b) > h     = b {bugAngle = Degrees 0  }
               | pointY (bugCoords b) < (-h)  = b {bugAngle = Degrees 180}
               | otherwise =  b {bugAngle = getAngle (playerCoords (player g)) (bugCoords b) }
        h = half height - 80
        w = half width  - 80

--updates highscore in gamestate
updateHighScores :: BHGameState -> BHGameState
updateHighScores g | isNewHighScore (score g) (highScores g) = 
                        g { highScores = addHighScore HighScore {
                            highScoreName = name g, 
                            highScoreScore = score g } (highScores g)}
                   | otherwise = g

--Calcualtes angle for a bug. Comparing to where the bug is and where the player is.
getAngle :: Point -> Point -> Degrees Float
getAngle pp pb =  Degrees (atan2(pointX pb - pointX pp) (pointY pb - pointY pp) * 180/pi)

-- Checks if bugs are out of the screen, removes them from the game, which results in -1 lives.
checkFledBugs :: BHGameState -> BHGameState
checkFledBugs g | fledBugs > 0 = g' { lives = l, bugs = bugs'}
                | otherwise = g
              where removeBug b | pointX (c b) < w && pointX (c b) > (-w) && pointY (c b) > (-h) && pointY (c b) < h = Just b
                                | otherwise = Nothing
                    fledBugs = length (mapMaybe bugsOutOfRange (bugs g))                   
                    c  = bugCoords
                    h  = half height + 10
                    w  = half width  + 10
                    g' = toggleBug g
                    l  = lives g - fledBugs
                    bugs' = mapMaybe removeBug (bugs g')
                    bugsOutOfRange b  | pointX (c b) > w 
                                        || pointX (c b) < (-w) 
                                        || pointY (c b) < (-h) 
                                        || pointY (c b) > h = Just b
                                      | otherwise = Nothing

--Checks if there are bugs left in the game, if not makes new ones.
checkZeroBugs :: BHGameState -> BHGameState
checkZeroBugs g | null (bugs g) = g' 
                | otherwise     = g
                  where (bs, r) = initRandomBugs (sprites g) (numberOfBugsForLevel lvl) (rng g) lvl
                        lvl = level g'
                        g' = g {
                          broken = False,
                          player = resetPlayer (player g),
                          level = level g + 1,
                          bugs = bs,
                          rng = r
                        }
                    
updateGameState :: Maybe Player -> Maybe [Bug] -> BHGameState -> BHGameState
updateGameState p b g = g {
  player = fromMaybe (player g) p,
  bugs = fromMaybe (bugs g) b
}

--called by updateScore, checks if a bug is hit.
updateScore' :: BHGameState -> Bug -> Maybe BHGameState
updateScore' g b | bx - (pt * 0.75) < px &&
                   bx + (pt * 0.75) > px &&
                   by - pt < py &&
                   by + pt > py = Just (g { score = score g + 1, bugs = bugs', bullets = bullets g ++ [initBullet (px, py)]})
                 | otherwise = Nothing
                 where (bx, by) = bugCoords b
                       (px, py) = playerCoords (player g) 
                       hp = bugHp b                   
                       bugs' = hitBug (bugs g) b 
                       pt  | fst (bugResource b) > 0.5 = 50 * fst (bugResource b)
                           | otherwise                 = 100 * fst (bugResource b)

-- called when player shoots/ pressed space.
updateScore :: BHGameState -> BHGameState
updateScore g | paused g || null hits = g    -- nothing happens to the score when paused or no bug is hit.
              | otherwise = head hits        -- if bugs are on some cordinates you can just kill 1.
                where hits = mapMaybe (updateScore' g) (bugs g) --check if bug(s) are hit.

--pause function, called when player presses 'p'
togglePaused :: BHGameState -> BHGameState
togglePaused g = g {paused = not (paused g)}

--Checks if there are dead bugs (hp <= 0), then animate there deaths
deadBugs :: BHGameState -> BHGameState
deadBugs g = g { bugs = mapMaybe newbug (bugs g)}
            where y' y = max (-(half height)) y-5
                  newbug b | fst (bugResource b) < 0.005 = Nothing
                           | bugHp b <= 0 = Just (b { bugCoords   = (fst (bugCoords b), y' (snd (bugCoords b))) ,
                                                      bugResource = (fst (bugResource b) * 0.95, snd (bugResource b)),
                                                      bugAngle    = bugAngle b + Degrees 10
                                                    })
                           | otherwise = Just b
                  

--When a bug escapes this functions triggers a "COMPUTER BUG"
toggleBug :: BHGameState -> BHGameState
toggleBug g | even (score g) = bugPlayerSpeed g'
            | otherwise      = bugScales g'
            where  g' = g {broken = True}

 -- "COMPUTER BUG"           
bugScales :: BHGameState -> BHGameState
bugScales g = g { bugs = map newbug (bugs g)}
             where newbug b = b {bugResource = (1.5, snd (bugResource b)) }
-- "COMPUTER BUG"
bugPlayerSpeed :: BHGameState -> BHGameState
bugPlayerSpeed g = g { 
     player = (player g) { defaultSpeed = 900 }         
                     }
--END OF GAME STATE
endOfGame :: BHGameState -> Bool
endOfGame g | lives g < 1 = True
            | otherwise   = False