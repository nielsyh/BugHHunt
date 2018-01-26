module BugHHunt.Utils where

import Data.Angle
import System.Random

-- unwraps the degrees datastructure
getDegrees :: Degrees x -> x
getDegrees (Degrees x) = x

-- returns a list of randoms between two floats
getRandoms :: (Float, Float) -> Int -> StdGen -> ([Float], StdGen)
getRandoms r 0 g = ([], g)
getRandoms r x g = (a : xs, ng)
  where (a, pg) = randomR r g
        (xs, ng) = getRandoms r (x - 1) pg