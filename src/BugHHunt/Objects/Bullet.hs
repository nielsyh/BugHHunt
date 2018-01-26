module BugHHunt.Objects.Bullet where

import Graphics.Gloss
import BugHHunt.Data

-- bullet datatype
data Bullet = Bullet {
  bulletCoords :: Point,
  bulletAnimFrame :: Float
}

-- bullet initializer
initBullet :: Point -> Bullet
initBullet p = Bullet {
  bulletCoords = p,
  bulletAnimFrame = 2
}

-- bullet draw function
instance Drawable Bullet where
  draw b = Translate (pointX (bulletCoords b)) (pointY (bulletCoords b)) $ Color red $ circleSolid (bulletSize b)

-- bullet update function
instance Updateable Bullet where
  uUpdate s b = b { bulletAnimFrame = bulletAnimLength s (bulletAnimFrame b)}

-- size of the bullet
bulletSize :: Bullet -> Float
bulletSize b = 10 * bulletAnimFrame b

-- remaining animation duration of the bullet
bulletAnimLength :: Float -> Float -> Float
bulletAnimLength seconds start = max 0 (start - seconds)