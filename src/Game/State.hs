{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.State (
  GameState(..),
  liftIO,
  Running(..),
  Game(..),
  get, put, modify,
  run, start, stop, toggleRun
    )where

import Control.Monad.Reader
import Data.Array
import Haskagon.Hexagon
import Haste.Graphics.Canvas (Point(..), Color(..))
import System.Random
import Control.Monad.State.Class
import Control.Monad.IO.Class (liftIO)
import Haste.Events (MonadEvent(..), mkHandler)
import Data.IORef

instance Random Color where
  random gen = randomR (RGB 0 0 0, RGB 255 255 255) gen
  randomR (RGB r1 g1 b1, RGB r2 g2 b2) gen
    = (RGB (r `mod` 256) (g `mod` 256) (b `mod` 256), gen3)
    where (r, gen1) = randomR (r1, r2) gen
          (g, gen2) = randomR (g1, g2) gen1
          (b, gen3) = randomR (b1, b2) gen2

instance Show Color where
  show = \case
    RGB  r g b   -> concat ["RGB ", show r, " ", show g, " ", show b]
    RGBA r g b a -> concat [show $ RGB r g b, " ", show a]

data Running = Running | Stopped
  deriving Show

data GameState = GameState {
  shape    :: Hexagon,
  hexagons :: [(Point, Color)],
  timer    :: Running
  } deriving Show

newtype Game a = Game {unG :: ReaderT (IORef GameState) IO a}
  deriving(Monad, Functor, Applicative, MonadReader (IORef GameState), MonadIO)

instance MonadState GameState Game where
  get = ask >>= liftIO . readIORef
  put x = ask >>= liftIO . flip writeIORef x

instance MonadEvent Game where
  mkHandler f = do
    ref <- ask
    return $ \a -> run' ref (f a)

run' :: IORef GameState -> Game () -> IO ()
run' stateRef g = runReaderT (unG g) stateRef

run :: Point -> Hexagon -> Game () -> IO ()
run p hex action  = do
  ref <- genState p hex >>= newIORef
  runReaderT (unG action) ref

genState :: Point -> Hexagon -> IO GameState
genState p h = GameState
               <$> return h
               <*> mapM pointCol (p:map (>+ p) (neighbours h))
               <*> return Running
  where pointCol p = (p,) <$> randomRIO
          (RGB 128 128 128, RGB 255 255 255)

start, stop, toggleRun :: Game ()
start = modify $ \st -> st {timer= Running}
stop = modify $ \st -> st {timer= Stopped}
toggleRun = modify $ \st -> st {timer= t $ timer st}
  where t Running = Stopped
        t Stopped = Running
