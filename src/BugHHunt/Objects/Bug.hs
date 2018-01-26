{-# LANGUAGE FlexibleInstances #-}

module BugHHunt.Objects.Bug where

import BugHHunt.Data
import BugHHunt.Graphics
import Graphics.Gloss
import Data.Angle
import BugHHunt.Utils

import System.Random

--bug datatype
data Bug = Bug {
  bugResource :: Resource,
  bugCoords :: Point,
  bugVelocity :: Int,
  bugAngle :: Degrees Float,
  bugHp :: Int
} deriving (Show)

--initialize a bug
initBug :: Sprites -> (Float, Float)-> Point -> Bug
initBug s (velocity, angle) (x,y) = Bug {
  bugResource = (0.3, bugSprite s),
  bugCoords = (x, y),
  bugVelocity = round (velocity * 10),
  bugAngle = Degrees (angle * 360),
  bugHp = 1
}

--help function for initbug
initBug' :: Sprites -> (Float, Float)-> Point -> Int-> Bug
initBug' s (velocity, angle) (x,y) l = Bug {
  bugResource = (0.8, bugSprite s),
  bugCoords = (x, y),
  bugVelocity = round (velocity * 10),
  bugAngle = Degrees (angle * 360),
  bugHp = 5 * l
}

--
numberOfBugsForLevel :: Int -> Int
numberOfBugsForLevel x | x `mod` 5 == 0 = 1 
                       | otherwise = 2 * x
                       
-- x == 1 Means masterbug
-- otherwise general bugs
initRandomBugs :: Sprites -> Int -> StdGen -> Int -> ([Bug], StdGen)
initRandomBugs s x g l | x == 1 = (map (\(point, av) -> initBug' s av point l) (zip points avs), g5)
                       | otherwise =  (map (\(point, av) -> initBug s av point) (zip points avs), g5)
  where (rx, g2) = getRandoms (-100, 100) x g
        (ry, g3) = getRandoms (-100, 100) x g2
        (ra, g4) = getRandoms (0, 360) x g3
        (rv, g5) = getRandoms (0, 1) x g4
        points = zip rx ry
        avs = zip (map (\v -> v * realToFrac x) rv) ra

moveBug :: Point -> Bug -> Bug
moveBug p b = Bug {
  bugResource = bugResource b,
  bugCoords = p,
  bugVelocity = bugVelocity b,
  bugAngle = bugAngle b,
  bugHp =bugHp b
}

--Compares all bugs with 1 bug, if found reduce HP with 1
hitBug :: [Bug] -> Bug -> [Bug]
hitBug xs b = map (\x -> if x == b then bg else x) xs
          where bg = b {bugHp = bugHp b - 1}

          --Drawable implementation for bug
instance Drawable Bug where
  draw b = translate (pointX c) (pointY c) pic
    where pic = rotate (getDegrees a) (resource (bugResource b))
          c   = bugCoords b
          a   = bugAngle b
-- Updatabe implementation for bug
instance Updateable Bug where
  uUpdate s b = moveBug coords b
    where coords = (x', y')
          x' = fst c + sine   a * (fromIntegral (bugVelocity b) * s)
          y' = snd c + cosine a * (fromIntegral (bugVelocity b) * s)
          c  = bugCoords b
          a  = bugAngle b

-- Checks if bugs are equal. compares location and HP.
instance Eq Bug where
  Bug _ a _ _ x == Bug _ b _ _ y =  a == b && x == y