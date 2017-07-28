module Game (eventState,
             validState,
             updateState,
             movePlayer,
             startPoint) where
import Map

import Haste.Graphics.Canvas
import Haste.Events

import Data.IORef
import Data.List

type State = (Point, Maybe HandlerInfo)

-- remove new click handle
-- reinstate the last handle
-- render the game again
eventWatcher :: (Point -> IO ()) -> IORef State -> MouseData -> IO ()
eventWatcher renderState stateRef _ = return ()

isEvent :: Tile -> Bool
isEvent (Event _) = True
isEvent _ = False

eventState :: Elem -> Map -> (Point -> IO ()) -> IORef State -> IO ()
eventState ce (c, tiles) renderState stateRef
  | isEvent (tiles !! i) = do
    (point, Just handle) <- readIORef stateRef
    unregisterHandler handle
    writeIORef stateRef ((x, y), Nothing)
    handle' <- onEvent
          ce
          Click $
          (eventWatcher renderState stateRef)
    writeIORef stateRef (point, Just handle')
                       
    -- render the text in the string
  | otherwise = return ()
  where
    i = floor $ x + c * y

validState :: Map -> State -> Maybe State
validState (c, tiles) ((x, y), h)
  | x < 0 || y < 0 || i >= length tiles = Nothing
  | tiles !! i  == Wall = Nothing
  | otherwise = Just $ ((x, y), h)
  where i = floor $ x + c * y

updateCoord :: Double -> Double -> Double -> Double
updateCoord threshhold dir old
  | dir > threshhold = old + 1
  | dir < -threshhold = old - 1
  | otherwise = old

updateState :: Double -> Double -> (Int, Int) -> State -> State
updateState width height (x, y) ((xS, yS), h)
  | abs xT > abs yT = ((updateCoord (width / 4) xT xS, yS), h)
  | otherwise = ((xS, updateCoord (height / 4) yT yS), h)
  where
    xT = fromIntegral x - width / 2
    yT = fromIntegral y - height / 2

movePlayer :: (Point -> IO ()) ->
              ((Int, Int) -> State -> Maybe State) ->
              (State -> IO ()) ->
              IORef State ->
              MouseData ->
              IO ()
movePlayer renderState
           updateAndValidateState
           eventStateCurried
           stateRef
           (MouseData mousePos _ _) = do
  state <- readIORef stateRef 
  case updateAndValidateState mousePos state of
    Just state' -> do 
      renderState $ fst state'
      writeIORef stateRef state'
      eventStateCurried state'
    Nothing -> return ()

startPoint :: Map -> Point
startPoint (c, tiles) = (x, y)
  where
    (Just i) = elemIndex Start tiles
    x = fromIntegral (mod i (floor c))
    y = fromInteger (floor ((realToFrac i) / c))
