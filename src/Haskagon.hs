{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Haste.DOM
import Haste.Events
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

createOptions :: Maybe Canvas -> IO ()
createOptions canvas = do
  let range :: Double -> Double -> IO Elem
      range min max = newElem "input" `with` [
        "type"  =: "range",
        "min"   =: show min,
        "max"   =: show max,
        "value" =: "0"
        ]
      button text = newElem "button" `with` ["textContent" =: text]
      container = newElem "div" `with` [
        style "position" =: "fixed",
        style "width" =: "200px",
        style "height" =: "200px",
        style "float" =: "right",
        style "top" =: "0px",
        style "right" =: "0px",
        style "background-color" =: "#CCC",
        "id" =: "options"
        ]

  [x, y] <- case canvas of Just c -> do
                             [w, h] <- mapM (getProp c) ["width", "height"]
                             sequence [range 0 $ read w, range 0 $ read h]
                           Nothing -> do
                             sequence [range 0 200, range 0 200]
  size     <- range 0 200
  rotation <- range 0 (2*pi) `with` ["step" =: show (pi / 30)]
  [newHex, clear]  <- mapM button ["Create new Hexagon", "Clear board"]
  opts <- container

  clear `onEvent` Click $ \_ -> case canvas of
    Just c -> render c $ return ()
    Nothing -> return ()

  mapM_ (appendChild opts) [x, y, size, rotation, newHex, clear]
  documentBody `appendChild` opts
  return ()

main = do
  canvas <- createAndAddCanvas
  createOptions canvas
  case canvas of
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
    stroke $ vectorLine myHex
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
