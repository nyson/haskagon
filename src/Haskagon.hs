{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Haste.DOM
import Haste.Events
import Haskagon.Hexagon
import Haste.Graphics.Canvas
import UI.Options

createAndAddCanvas :: IO (Maybe Canvas)
createAndAddCanvas = do
  c <- newElem "canvas" `with` [
    "height" =: "500",
    "width" =: "500"
    ]
  documentBody `appendChild` c
  fromElem c

main = do
  canvas <- createAndAddCanvas
  case canvas of
    Just c -> runProg c
    Nothing -> putStrLn "no!"


runProg canvas = do
  createOptions canvas

grid :: Vector -> Point -> Vector -> Picture ()
grid (ix, iy) (maxx, maxy) (ox, oy) = mapM_ stroke [horizlines, vertilines]
  where
    vertilines = mapM_ (\x -> line (ox, 0) (ox, maxx))
      [x*ix | x <- [0 .. ceil maxx]]
    horizlines = mapM_ (\y -> line (0, oy) (maxy, oy))
      [y*iy | y <- [0 .. ceil maxy]]

    ceil :: Double -> Double
    ceil = fromIntegral . ceiling
