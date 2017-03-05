{-# LANGUAGE LambdaCase #-}
module Game.Loop where

import Haste
import Control.Monad (void)
import Game.State

loop :: Game () -> Game ()
loop action = timer <$> get >>= \case
  Running -> void $ setTimer (Once 16) (action >> loop action)
  Stopped -> void $ setTimer (Once 16) (return () >> loop action)

