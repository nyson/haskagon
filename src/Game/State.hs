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
import Control.Monad.Random (runRand, getRandomR)
import Data.Array
import Haskagon.Hexagon
import Haste.Graphics.Canvas (Point(..), Color(..))
import System.Random
import Control.Monad.State.Class
import Control.Monad.IO.Class (liftIO)
import Haste.Events (MonadEvent(..), mkHandler)
import Data.IORef

instance Random Color where
  random = randomR (RGB 0 0 0, RGB 255 255 255)
  randomR (RGB r1 g1 b1, RGB r2 g2 b2) gen
    = flip runRand gen $ toRGB <$> mapM getRandomR [(r1,r2), (g1,g2), (b1,b2)]
    where toRGB = (\[r, g, b] -> RGB r g b) . map (`mod` 256)

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
  put value = ask >>= liftIO . flip writeIORef value

instance MonadEvent Game where
  mkHandler f = runWithRef <$> ask
    where runWithRef ref = run' ref . f

run' :: IORef GameState -> Game () -> IO ()
run' stateRef g = runReaderT (unG g) stateRef

run :: Point -> Hexagon -> Game () -> IO ()
run p hex action = genState p hex >>= runReaderT (unG action)

genState :: Point -> Hexagon -> IO (IORef GameState)
genState p h = newIORef =<< GameState
               <$> return h
               <*> mapM pointCol (p:map (>+ p) (neighbours h))
               <*> return Running
  where pointCol p = (p,) <$> randomRIO (RGB 128 128 128, RGB 255 255 255)

start, stop, toggleRun :: Game ()
start     = modify $ \st -> st {timer= Running}
stop      = modify $ \st -> st {timer= Stopped}
toggleRun = modify $ \st -> st {timer= t $ timer st}
  where t Running = Stopped
        t Stopped = Running
