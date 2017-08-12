module Game (play) where

import Graphics

import Haste
import Haste.DOM
import Haste.Events

import Data.IORef
import Data.List

-- Point of player and a boolean if the player is moving
data Mode = Normal | Locked deriving (Eq, Show)
data State = State Point Mode deriving (Eq, Show)

validPoint :: Map -> Point -> Maybe Point
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

startPoint :: Map -> Point
startPoint (c, tiles) = (x, y)
  where
    (Just i) = elemIndex Start tiles
    x = fromIntegral (mod i (floor c))
    y = fromInteger (floor ((realToFrac i) / c))


---------------------------------Impure-------------------------------
eventPoint :: Map -> [MapInput] -> (String -> IO ()) -> Point -> IO ()
eventPoint (c, tiles) mapInput outputText (x, y) = outputText tileString
  where
    tile = tiles !! floor (x + c * y)
    tileString = maybe "" snd (lookup tile mapInput)

animateMovePlayer :: IORef State -> (Point -> IO()) -> [Point] -> IO ()
animateMovePlayer stateRef _ []  = do
  State p _ <- readIORef stateRef
  writeIORef stateRef (State p Normal)
  return ()
animateMovePlayer stateRef renderState' (nextState:xs)  = do
  renderState' nextState
  setTimer (Once 10) (animateMovePlayer stateRef renderState' xs)
  return ()

onClick :: (Point -> IO ()) ->
           ((Int, Int) -> Point -> Maybe Point) ->
           (Point -> IO ()) ->
           IORef State ->
           MouseData ->
           IO ()
onClick renderState'
        updateAndValidatePoint
        eventPointCurried
        stateRef
        (MouseData mousePos _ _) = do
  State p gameState <- readIORef stateRef 
  case gameState of
    Normal -> do
      case updateAndValidatePoint mousePos p of
        Just p' -> do 
          writeIORef stateRef (State p' Locked)
          animateMovePlayer stateRef renderState' (interPoints p p')
          eventPointCurried p'
        Nothing -> return ()
    Locked -> return ()

play :: Maybe String -> IO ()
play (Just mapStr) = do 
  case parseMap mapStr of
    Just (m, mInput) -> do
      Just ce <- elemById "canvas"
      Just c <- fromElem ce
      Just outputElem <- elemById "output"
      imgs <- loadImages mInput

      stateRef <- newIORef $ State (startPoint m) Normal
      onEvent ce
              Click $
              onClick (renderState c (drawMap imgs m)) 
                      (((.) (validPoint m)) . updatePoint)
                      (eventPoint m mInput (changeInnerHTML outputElem))
                         stateRef
      renderState c (drawMap imgs m) (startPoint m)
      eventPoint m mInput (changeInnerHTML outputElem) (startPoint m)
    Nothing -> alert "Map parsing error"
play Nothing = alert "Map file not loaded"

changeInnerHTML :: Elem -> String -> IO ()
changeInnerHTML e s = setProp e "innerHTML" s
