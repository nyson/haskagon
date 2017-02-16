module Haskagon.ToShape where

import Haste.Graphics.Canvas

class ToShape a where
  toShape :: Point -> a -> Shape ()

instance ToShape ts => ToShape [ts] where
  toShape p shapes = mapM_ (toShape p) shapes
