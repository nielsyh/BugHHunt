module BugHHunt.Data where

import System.Random

import Control.Arrow

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap

type Resource = (Float, Picture)

resource :: Resource -> Picture -- resource, scale/size picture
resource (s, p) = scale s s p

pointX :: Point -> Float -- get x cordinate of a point
pointX (x, _) = x

pointY :: Point -> Float -- get y cordinate of a point
pointY (_, y) = y

--typeclass Drawable
class Drawable d where
  draw :: d -> Picture

--typeclass Moveable
class Moveable m where
  mTranslate :: Point -> m -> m
  
--typeclass Updateable
class Updateable u where
  uUpdate :: Float -> u -> u

instance (Drawable t) => Drawable [t] where
  draw x = pictures (map draw x)

instance (Updateable t) => Updateable [t] where
  uUpdate s = map (uUpdate s)
