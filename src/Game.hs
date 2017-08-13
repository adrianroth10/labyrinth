module Game (play) where

import Graphics

import Haste
import Haste.DOM
import Haste.Events

import Data.IORef
import Data.List

-- Point of player and a boolean if the player is moving
type MapState = (Point, MapType, Picture ())
data Mode = Normal | Locked deriving Eq
data State = State Mode MapState

validPoint :: MapType -> Point -> Maybe Point
validPoint (c, tiles) (x, y)
  | x < 0 || y < 0 || x >= c || i >= length tiles = Nothing
  | eqTile (tiles !! i) (Wall 1) = Nothing
  | otherwise = Just $ (x, y)
  where i = floor $ x + c * y

updateCoord :: Double -> Double -> Double -> Double
updateCoord threshhold dir old
  | dir > threshhold = old + 1
  | dir < -threshhold = old - 1
  | otherwise = old

updatePoint :: (Int, Int) -> Point -> Point
updatePoint (x, y) (xS, yS)
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

startPoint :: MapType -> Point
startPoint (c, tiles) = (x, y)
  where
    (Just i) = elemIndex Start tiles
    x = fromIntegral (mod i (floor c))
    y = fromInteger (floor ((realToFrac i) / c))


---------------------------------Impure-------------------------------
eventPoint :: MapType -> [MapInput] -> (String -> IO ()) -> Point -> IO ()
eventPoint (c, tiles) mapInput outputText (x, y) = outputText tileString
  where
    tile = tiles !! floor (x + c * y)
    tileString = maybe "" snd (lookup tile mapInput)

animateMovePlayer :: IORef State -> (Point -> IO ()) -> [Point] -> IO ()
animateMovePlayer stateRef _ []  = do
  State _ (p, m, mPicture) <- readIORef stateRef
  writeIORef stateRef $ State Normal (p, m, mPicture)
  return ()
animateMovePlayer stateRef renderState' (nextState:xs)  = do
  renderState' nextState
  setTimer (Once 10) (animateMovePlayer stateRef renderState' xs)
  return ()

movePlayer :: [MapInput] -> MapState -> IORef State -> (Int, Int) -> IO ()
movePlayer mInput (p, m, mPicture) stateRef mousePos = do
  case validPoint m (updatePoint mousePos p) of
    Just p' -> do 
      writeIORef stateRef $ State Locked (p', m, mPicture)
      animateMovePlayer stateRef (renderState mPicture) (interPoints p p')
      eventPoint m mInput changeOutputHTML p'
    Nothing -> return ()
  

onClick :: [MapInput] -> [(Tile, Bitmap)] ->
           IORef State -> MouseData -> IO ()
onClick mInput _ stateRef (MouseData mousePos _ _) = do
  State gameState mapState <- readIORef stateRef 
  case gameState of
    Normal -> movePlayer mInput mapState stateRef mousePos
    Locked -> return ()

play :: Maybe String -> IO ()
play (Just mapStr) = do 
  case parseMap mapStr of
    Just (m, mInput) -> do
      Just ce <- elemById "canvas"
      imgs <- loadImages mInput

      stateRef <- newIORef $ State Normal
                                  ((startPoint m), m, (drawMap imgs m))
      onEvent ce
              Click $
              onClick mInput imgs stateRef
      renderState (drawMap imgs m) (startPoint m)
      eventPoint m mInput changeOutputHTML (startPoint m)
    Nothing -> alert "Map parsing error"
play Nothing = alert "Map file not loaded"

changeOutputHTML :: String -> IO ()
changeOutputHTML s = do
  Just e <- elemById "output"
  setProp e "innerHTML" s
