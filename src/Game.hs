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

type State = Point

tileString :: Tile -> String
tileString (Event s) = s
tileString _ = ""

eventState :: Map -> (String -> IO ()) -> State -> IO ()
eventState (c, tiles) outputText (x, y) = outputText $ tileString $ tiles !! i
  where i = floor $ x + c * y

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
      renderState $ state'
      writeIORef stateRef state'
      eventStateCurried state'
    Nothing -> return ()

startPoint :: Map -> State
startPoint (c, tiles) = (x, y)
  where
    (Just i) = elemIndex 1 $ map unTile tiles
    x = fromIntegral (mod i (floor c))
    y = fromInteger (floor ((realToFrac i) / c))