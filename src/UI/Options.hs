{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module UI.Options where

import Data.Maybe (fromJust)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Haskagon.Hexagon
import Haskagon.ToShape

crosshair :: Point -> Color -> Hexagon -> Picture ()
crosshair p col hex = undefined

label :: String -> Elem -> IO Elem
label text elem = do
  label <- do l <- newElem "label" `with` ["textContent" =: text]
              getAttr elem "id" >>= \case
                ""     -> return l
                elemId -> return l `with` ["for" =: elemId]
  newElem "div" `with` [children [label, elem]]

range :: Double -> Double -> IO Elem
range min max = newElem "input" `with` [
  "type"  =: "range",
  "min"   =: show min,
  "max"   =: show max,
  "value" =: "0"
  ]

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

button :: String -> IO Elem
button text = newElem "button" `with` ["textContent" =: text]

createOptions :: Canvas -> IO ()
createOptions canvas = do
  [w, h] <- mapM (getProp canvas) ["width", "height"]
  x <- range 0 (read w) `with` ["value" =: show (read w / 2)]
  y <- range 0 (read h) `with` ["value" =: show (read h / 2)]
  size     <- range 0 200 `with` ["value" =: "100"]
  rotation <- range 0 (2*pi + pi/60) `with` [
    "step" =: show (pi / 30),
    "id" =: "rotation_slider"
    ]
  clear <- button "Clear board"
  opts <- container

  let redrawHex = do
        [xpos, ypos, s, r] <- map fromJust <$> mapM getValue
          [x, y, size, rotation]
        render canvas $ stroke $ do
          let p = (read xpos, read ypos)
              hex = Hexagon (read s) (read r)
          toShape    p hex
          vectorLine p hex

  redrawHex

  clear `onEvent` Click     $ \_ -> render canvas $ return ()
  opts  `onEvent` MouseMove $ \_ -> redrawHex

  elems <- (++ [clear]) <$> sequence [
    label "X: "      x,
    label "Y: "      y,
    label "Size: "   size,
    label "Rotate: " rotation
    ]
  mapM_ (appendChild opts) elems
  documentBody `appendChild` opts

