{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Haste.DOM
import Haskagon.Hexagon
import Haste.Graphics.Canvas 

createAndAddCanvas :: IO (Maybe Canvas)
createAndAddCanvas = do
  c <- newElem "canvas" `with` [
    "height" =: "500",
    "width" =: "500"
    ]
  documentBody `appendChild` c
  fromElem c

main = createAndAddCanvas >>= \case
  Just c -> runProg c
  Nothing -> putStrLn "no!"


runProg canvas = do
  let scale = 2
      myHex = Hexagon (50 * scale, 50 * scale) (50 * scale) 0
      corners = center myHex:[corner myHex (0 + (pi/3) * x) | x <- [0..5]]
      ps = pies $ Hexagon (50 * scale, 50 * scale) (50*scale) 0
      offset = (width myHex, width myHex - height myHex)
      canvasSize = (500, 500)

  print $ width myHex
  render canvas $ do
    grid (width myHex, height myHex) canvasSize offset
    setFillColor (RGB 255 0   255)
    fill $ shape myHex
    mapM_ stroke $ pies myHex
    fill $ mapM_ (flip circle 5) corners
    stroke $ mapM_ (flip circle 5) corners
    setFillColor (RGB 0 0 0)
    mapM_ (\(x, y) -> text (x + 10, y + 10) $ show (r x, r y)) $ corners
      where
        r :: Double -> Double
        r = (/ 100) . fromIntegral . round . (* 100)

grid :: Vector -> Point -> Vector -> Picture ()
grid (ix, iy) (maxx, maxy) (ox, oy) = mapM_ stroke [horizlines, vertilines]
  where
    vertilines = mapM_ (\x -> line (ox, 0) (ox, maxx))
      [x*ix | x <- [0 .. ceil maxx]]
    horizlines = mapM_ (\y -> line (0, oy) (maxy, oy))
      [y*iy | y <- [0 .. ceil maxy]]

    ceil :: Double -> Double
    ceil = fromIntegral . ceiling
