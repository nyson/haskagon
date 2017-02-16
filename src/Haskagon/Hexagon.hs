module Haskagon.Hexagon (
  ToShape(..),
  Pie(..),
  Hexagon(..),
  corner, side,
  pie, pies,
  height, width,
  (<>+), (>*), (>+),
  vectorLine
  ) where

import Haste.Graphics.Canvas
import Haskagon.ToShape

type Line = (Point, Point)
type Size = Double

data Hexagon = Hexagon Size Angle
data Pie = Pie Vector Vector

instance ToShape Hexagon where
  toShape p h = path $ map (>+ p) [corner h ((pi/3) * x) | x <- [0..6]]

instance ToShape Pie where
  toShape p (Pie c1 c2) = path [p, c2 >+ p, c1 >+ p, p]

-- | Corner of the hexagon
corner :: Hexagon -> Angle -> Point
corner (Hexagon size ha) angle = (size * cos (ha + angle),
                                  size * sin (ha + angle))

-- | Line of a side of the hexagon
side :: Hexagon -> Angle -> Line
side hex angle = (corner hex angle, corner hex $ angle + pi/3)

pie :: Hexagon -> Angle -> Pie
pie hex angle = Pie c1 c2
  where (c1, c2) = side hex angle

-- | Return a list with 6 triangels shaping the hexagon
pies :: Hexagon -> [Pie]
pies hex = map pieside [pi/3 * x | x <- [0, 2..5]]
  where pieside angle = pie hex angle

-- | Height of the hexagon
height :: Hexagon -> Double
height (Hexagon size _) = size * sin (pi / 3) - size * sin (pi * (5/3))

-- | With of the hexagon 
width :: Hexagon -> Double
width (Hexagon size _) = size * cos 0 - size * cos pi

-- | Rotate a hexagon
(<>+) :: Hexagon -> Angle -> Hexagon
(Hexagon size ha) <>+ a = Hexagon size $ ha + a

-- | Get a vector by applying a scalar to an angle
(>*) :: Double -> Angle -> Vector
s >* a = (s * cos a, s * sin a)

-- | Adding a point by a vector to find the resulting point
(>+) :: Point -> Vector -> Point
(x, y) >+ (dx, dy) = (x + dx, y + dy)

-- | Creating a vector from the hexagons starting angle
vectorLine :: Point -> Hexagon -> Shape ()
vectorLine p (Hexagon s a) = do
  let top = p >+ ((s*1.5) >* a)
  line p top
  circle top (s*0.1)

