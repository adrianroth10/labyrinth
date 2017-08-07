module Game (eventPoint,
             validPoint,
             updatePoint,
             movePlayer,
             startPoint) where
import Map

import Haste
import Haste.Graphics.Canvas
import Haste.Events

import Data.IORef
import Data.List

-- Point of player and a boolean if the player is moving
type State = (Point, Bool)

tileString :: Tile -> String
tileString (Event s) = s
tileString (Start s) = s
tileString (End s) = s
tileString _ = ""

eventPoint :: Map -> (String -> IO ()) -> Point -> IO ()
eventPoint (c, tiles) outputText (x, y) = outputText $ tileString $ tiles !! i
  where i = floor $ x + c * y

validPoint :: Map -> Point -> Maybe Point
validPoint (c, tiles) (x, y)
  | x < 0 || y < 0 || x >= c || i >= length tiles = Nothing
  | tiles !! i  == Wall = Nothing
  | otherwise = Just $ (x, y)
  where i = floor $ x + c * y

updateCoord :: Double -> Double -> Double -> Double
updateCoord threshhold dir old
  | dir > threshhold = old + 1
  | dir < -threshhold = old - 1
  | otherwise = old

updatePoint :: Double -> Double -> (Int, Int) -> Point -> Point
updatePoint width height (x, y) (xS, yS)
  | abs xT > abs yT = (updateCoord (width / 4) xT xS, yS)
  | otherwise = (xS, updateCoord (height / 4) yT yS)
  where
    xT = fromIntegral x - width / 2
    yT = fromIntegral y - height / 2

nInterPoints :: Double
nInterPoints = 5
interPoints :: Point -> Point -> [Point]
interPoints (x1, y1) (x2, y2) = (take (floor nInterPoints) $ zip
                                                (iterate (+xdiff) x1)
                                                (iterate (+ydiff) y1))
                                                  ++ [(x2, y2)]
  where
    xdiff = (x2 - x1) / nInterPoints
    ydiff = (y2 - y1) / nInterPoints

animateMovePlayer :: IORef State -> (Point -> IO()) -> [Point] -> IO ()
animateMovePlayer stateRef _ []  = do
  (p, _) <- readIORef stateRef
  writeIORef stateRef (p, False)
  return ()
animateMovePlayer stateRef renderState (nextState:xs)  = do
  renderState nextState
  setTimer (Once 10) (animateMovePlayer stateRef renderState xs)
  return ()

movePlayer :: (Point -> IO ()) ->
              ((Int, Int) -> Point -> Maybe Point) ->
              (Point -> IO ()) ->
              IORef State ->
              MouseData ->
              IO ()
movePlayer renderState
           updateAndValidatePoint
           eventPointCurried
           stateRef
           (MouseData mousePos _ _) = do
  (p, moving) <- readIORef stateRef 
  case moving of
    False ->
      case updateAndValidatePoint mousePos p of
        Just p' -> do 
          writeIORef stateRef (p', True)
          animateMovePlayer stateRef renderState (interPoints p p')
          eventPointCurried p'
        Nothing -> return ()
    True -> return ()

startPoint :: Map -> Point
startPoint (c, tiles) = (x, y)
  where
    (Just i) = elemIndex 1 $ map unTile tiles
    x = fromIntegral (mod i (floor c))
    y = fromInteger (floor ((realToFrac i) / c))
