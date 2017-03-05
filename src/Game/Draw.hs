module Game.Draw where

redraw Options = do
  [xpos, ypos, s, r] <- map fromJust <$> mapM getValue
    [x, y, size, rotation]
        let p = (read xpos, read ypos)
            hex = Hexagon (read s) (read r)

        pics <- flip mapM (neighbours hex) $ \p' -> do
          col <- randomIO -- randomCol
          return $ do
            setFillColor col
            fill $ toShape (p >+ p') hex

        render canvas $ sequence pics

        renderOnTop canvas $ do
          setFillColor $ RGB 100 100 100
          fill $ do
            toShape p hex
            vectorLine p hex

  redrawHex
