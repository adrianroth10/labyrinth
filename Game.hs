module Game (State(x_coord, y_coord),
             movePlayer,
             startState) where
import Graphics
import Map

import Haste.Graphics.Canvas
import Haste.Events

import Data.IORef
import Data.List

data State = State {
    x_coord :: Double,
    y_coord :: Double
  }

movePlayer :: Canvas -> Picture () -> IORef State -> MouseData -> IO ()
movePlayer canvas picture stateRef (MouseData (x, y) _ _) = do
  state <- readIORef stateRef 
  let newState = State (new_coord x width (x_coord state))
                       (new_coord y height (y_coord state))
  render canvas $ do 
    translateMap (x_coord state) (y_coord state) picture
    drawPlayer
  writeIORef stateRef newState
  where
    new_coord mouse size old =
          old +
          signum (fromIntegral mouse - size / 2) *
          boolToDouble (abs (fromIntegral mouse - size / 2) > 50)
    boolToDouble False = 0
    boolToDouble True = 1

startState :: Map -> State
startState (c, tiles) = State x y
  where
    (Just i) = elemIndex Start tiles
    x = fromIntegral (mod i (floor c))
    y = fromInteger (floor ((realToFrac i) / c))
