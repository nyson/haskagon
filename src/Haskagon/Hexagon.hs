module Haskagon.Hexagon where

import Haste.Graphics.Canvas
import Debug.Trace

type Line = (Point, Point)
type Polygon = [Point]
type Size = Double

data Hexagon = Hexagon Point Size Angle

corner :: Hexagon -> Angle -> Point
corner (Hexagon (x,y) size ha) angle =
  (x + size * cos (ha + angle), y + size * sin (ha + angle))

side :: Hexagon -> Angle -> Line
side hex angle = (corner hex angle, corner hex $ angle + pi/3)

center :: Hexagon -> Point
center (Hexagon p _ _) = p

pie :: Hexagon -> Angle -> Shape ()
pie hex angle = path [c1, center hex, c2]
  where (c1, c2) = side hex angle

-- | Return a list with 6 triangels shaping the hexagon
pies :: Hexagon -> [Shape ()]
pies hex = map side [pi/3 * x | x <- [0, 2..5]]
  where side angle = pie hex angle

-- | Create a shape from the hexagon
shape :: Hexagon -> Shape ()
shape h = path [corner h ((pi/3) * x) | x <- [0..5]]

-- | Heeeee?
height :: Hexagon -> Double
height (Hexagon _ size _) = size * sin (pi / 3) - size * sin (pi * (5/3))

width :: Hexagon -> Double
width (Hexagon _ size _) = size * cos 0 - size * cos pi

-- | Rotate a hexagon
(<>+) :: Hexagon -> Angle -> Hexagon
(Hexagon p size ha) <>+ a = Hexagon p size $ ha + a

-- | Get a vector by applying a scalar to an angle
(>*) :: Double -> Angle -> Vector
s >* a = (s * cos a, s * sin a)

(>+) :: Point -> Vector -> Point
(x, y) >+ (dx, dy) = (x + dx, y + dy)

vectorLine :: Hexagon -> Shape ()
vectorLine (Hexagon p s a) = do
  let top = p >+ ((s*1.5) >* a)
  line p top
  circle top 10

