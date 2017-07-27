module Game (validState,
             updateState,
             movePlayer,
             startState) where
import Map

import Haste.Graphics.Canvas
import Haste.Events

import Data.IORef
import Data.List

type State = Point

validState :: Map -> State -> Maybe State
validState (c, tiles) (x, y)
  | x < 0 || y < 0 || i >= length tiles = Nothing
  | tiles !! i  == Wall = Nothing
  | otherwise = Just $ (x, y)
  where i = floor $ x + c * y

updateCoord :: Double -> Double -> Double -> Double
updateCoord threshhold dir old
  | dir > threshhold = old + 1
  | dir < -threshhold = old - 1
  | otherwise = old

updateState :: Double -> Double -> (Int, Int) -> State -> State
updateState width height (x, y) (xS, yS)
  | abs xT > abs yT = (updateCoord (width / 4) xT xS, yS)
  | otherwise = (xS, updateCoord (height / 4) yT yS)
  where
    xT = fromIntegral x - width / 2
    yT = fromIntegral y - height / 2

movePlayer :: (State -> IO ()) ->
              ((Int, Int) -> State -> Maybe State) ->
              IORef State ->
              MouseData ->
              IO ()
movePlayer renderState
           updateAndValidateState
           stateRef
           (MouseData mousePos _ _) = do
  state <- readIORef stateRef 
  case updateAndValidateState mousePos state of
    Just state' -> do 
      renderState state'
      writeIORef stateRef state'
    Nothing -> return ()

startState :: Map -> State
startState (c, tiles) = (x, y)
  where
    (Just i) = elemIndex Start tiles
    x = fromIntegral (mod i (floor c))
    y = fromInteger (floor ((realToFrac i) / c))
