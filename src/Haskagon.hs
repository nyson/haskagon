{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Haste.DOM
import Haste.Events
import Haskagon.Hexagon
import Haste.Graphics.Canvas
import UI.Options
import Haste.Foreign (constant)
import Game.Loop
import Game.State

createAndAddCanvas :: IO (Maybe Canvas)
createAndAddCanvas = do
  let window = constant "window" :: Elem
  [w, h] <- mapM (getProp window)
    ["innerWidth", "innerHeight"]
  c <- newElem "canvas" `with` [
    "height" =: h,
    "width"  =: w
    ]
  documentBody `appendChild` c
  fromElem c

main = do
  documentBody `set` [style "margin"   =: "0px",
                      style "padding"  =: "0px",
                      style "overflow" =: "hidden"]
  createAndAddCanvas >>= \case
    Just c -> runProg c
    Nothing -> putStrLn "no!"

runProg canvas = do
  p <- start <$> mapM (getProp canvas) ["width", "height"]
  run p (Hexagon 100 0) $ do
    readOptions <- createOptions canvas
    loop (readOptions >> redraw)

  where start [x,y] = (read x / 2, read y / 2)
        redraw = do
          st <- get
          liftIO $ render canvas $ flip mapM_ (hexagons st) $ \(p, col) -> do
            setFillColor col
            fill . toShape p . shape $ st
