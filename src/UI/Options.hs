{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module UI.Options where

import Data.Maybe (fromJust)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Haskagon.Hexagon
import Haskagon.ToShape
import System.Random
import Game.State

data Options = Options

crosshair :: Point -> Color -> Hexagon -> Picture ()
crosshair p col hex = undefined

label :: String -> Elem -> Game Elem
label text elem = liftIO $ do
  label <- do l <- newElem "label" `with` ["textContent" =: text]
              getAttr elem "id" >>= \case
                ""     -> return l
                elemId -> return l `with` ["for" =: elemId]
  newElem "div" `with` [children [label, elem]]

range :: Double -> Double -> Game Elem
range min max = liftIO $ newElem "input" `with` [
  "type"  =: "range",
  "min"   =: show min,
  "max"   =: show max,
  "value" =: "0"
  ]

container :: Game Elem
container = liftIO $ newElem "div" `with` [
  style "position" =: "fixed",
  style "width" =: "200px",
  style "height" =: "200px",
  style "float" =: "right",
  style "top" =: "0px",
  style "right" =: "0px",
  style "background-color" =: "#CCC",
  "id" =: "options"
  ]

button :: String -> Game Elem
button text = liftIO $ newElem "button" `with` ["textContent" =: text]

createOptions :: Canvas -> Game (Game ())
createOptions canvas = do
  [w, h] <- liftIO $ mapM (getProp canvas) ["width", "height"]
  x <- range 0 (read w) `with` ["value" =: show (read w / 2)]
  y <- range 0 (read h) `with` ["value" =: show (read h / 2)]
  size     <- range 0 200 `with` ["value" =: "100"]
  rotation <- range 0 (2*pi + pi/60) `with` [
    "step" =: show (pi / 30),
    "id" =: "rotation_slider"
    ]
  toggleRendering <- button "Stop rendering"
  opts <- container

  elems <- (++ [toggleRendering]) <$> sequence [
    label "X: "      x,
    label "Y: "      y,
    label "Size: "   size,
    label "Rotate: " rotation
    ]
  liftIO $ mapM_ (appendChild opts) elems
  liftIO $ documentBody `appendChild` opts
  col <- liftIO $ randomRIO (RGB 0 0 0, RGB 128 128 128)

  toggleRendering `onEvent` Click $ \_ -> do
    t <- timer <$> get
    liftIO $ putStrLn $ "state before: " ++ show t
    toggleRun

    t' <- timer <$> get
    liftIO $ putStrLn $ "state after: " ++ show t'
    newText <- timer <$> get >>= \case
      Running -> return "Stop rendering"
      Stopped -> return "Start rendering"
    liftIO $ setProp toggleRendering "innerHTML" newText

  return $ do
    [xpos, ypos, s, r] <- liftIO $ map fromJust <$> mapM getValue
      [x, y, size, rotation]
    modify $ \st -> st {shape = Hexagon s r}

