module Game (eventState,
             validState,
             updateState,
             movePlayer,
             startPoint) where
import Map

import Haste
import Haste.Graphics.Canvas
import Haste.Events

import Data.IORef
import Data.List

type State = Point

tileString :: Tile -> String
tileString (Event s) = s
tileString (Start s) = s
tileString (End s) = s
tileString _ = ""

eventState :: Map -> (String -> IO ()) -> State -> IO ()
eventState (c, tiles) outputText (x, y) = outputText $ tileString $ tiles !! i
  where i = floor $ x + c * y

validState :: Map -> State -> Maybe State
validState (c, tiles) (x, y)
  | x < 0 || y < 0 || x >= c || i >= length tiles = Nothing
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

nInterPoints :: Double
nInterPoints = 10
interPoints :: Point -> Point -> [Point]
interPoints (x1, y1) (x2, y2) = (take (floor nInterPoints) $ zip
                                                (iterate (+xdiff) x1)
                                                (iterate (+ydiff) y1))
                                                  ++ [(x2, y2)]
  where
    xdiff = (x2 - x1) / nInterPoints
    ydiff = (y2 - y1) / nInterPoints

animateMovePlayer :: (Point -> IO()) -> [Point] -> IO ()
animateMovePlayer _ []  = return ()
animateMovePlayer renderState (nextState:xs)  = do
  renderState nextState
  setTimer (Once 5) (animateMovePlayer renderState xs)
  return ()

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
      writeIORef stateRef state'
      animateMovePlayer renderState (interPoints state state')
      eventStateCurried state'
    Nothing -> return ()

startPoint :: Map -> State
startPoint (c, tiles) = (x, y)
  where
    (Just i) = elemIndex 1 $ map unTile tiles
    x = fromIntegral (mod i (floor c))
    y = fromInteger (floor ((realToFrac i) / c))
