module Haskagon.Grid where

import Data.Array
import Haskagon.Hexagon
import Haste.Graphics.Canvas
import System.Random
data Grid a = Array (Int, Int) a
type GColor = Grid Color

instance Random Color where
  random g = randomR (RGB 0 0 0, RGB 255 255 255) g
  randomR (RGB ra ga ba, RGB rb gb bb) g = (RGB r g b, g3)
    where (r, g1) = randomR (minr, maxr) g
          (g, g2) = randomR (ming, maxg) g1
          (b, g3) = randomR (minb, maxb) g2
          dwn x = if x < 0   then 0   else x
          up x = if x > 255 then 255 else x
          (minr, ming, minb) = (dwn ra, dwn rb, dwn rg)
          (maxr, maxg, maxb) = (up  rb, up  gb, up  bb)

newRandomGrid :: (Int, Int) -> IO a -> Grid a
newRandomGrid (maxx,maxy) action
  = mapM filler [(x, y) | x <- [0..maxx], y <- [0..maxy]]
  where filler p = do
          val <- action
          return (p, val)
