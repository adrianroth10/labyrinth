module Game (play) where

import World
import Graphics

import Haste
import Haste.DOM
import Haste.Events

import Data.IORef
import Data.List

type MapState = (MapContent', Point, Picture ())
data Mode = Normal | Locked deriving Eq
type State = (Mode, MapState)

validPoint :: MapContent' -> Point -> Maybe Point
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

startPoint :: MapContent' -> Point
startPoint (c, tiles) = (x, y)
  where
    (Just i) = elemIndex Start tiles
    x = fromIntegral (mod i (floor c))
    y = fromInteger (floor ((realToFrac i) / c))

startMap :: World -> MapContent'
startMap [] = error "No start found"
startMap ((_, MapContent (c, tiles)):xti)
  | elem Start tiles = (c, tiles)
  | otherwise = startMap xti
startMap (_:xti) = startMap xti

---------------------------------Impure-------------------------------
eventPoint' :: EventItem -> IORef State -> IO ()
eventPoint' (EventItemList _) _ = return ()
eventPoint' (Text _) _ = return ()
eventPoint' (HTMLText s) _ = changeOutputHTML s
eventPoint' NoEvent _ = changeOutputHTML ""


eventPoint :: MapContent' -> World -> Point -> IORef State -> IO ()
eventPoint (c, tiles) world (x, y) stateRef = eventPoint' e stateRef
  where
    tile = tiles !! floor (x + c * y)
    Just (TileItem _ e) = lookup tile world

animateMovePlayer :: World -> IORef State -> (Point -> IO ()) ->
                     [Point] -> IO ()
animateMovePlayer world stateRef _ []  = do
  (_, (m, p, mPicture)) <- readIORef stateRef
  writeIORef stateRef (Normal, (m, p, mPicture))
  eventPoint m world p stateRef
  return ()
animateMovePlayer world stateRef renderState' (nextState:xs)  = do
  renderState' nextState
  setTimer (Once 10) (animateMovePlayer world stateRef renderState' xs)
  return ()

movePlayer :: World -> MapState -> IORef State -> (Int, Int) -> IO ()
movePlayer world (m, p, mPicture) stateRef mousePos = do
  case validPoint m (updatePoint mousePos p) of
    Just p' -> do 
      writeIORef stateRef (Locked, (m, p', mPicture))
      animateMovePlayer world stateRef
                        (renderState mPicture) (interPoints p p')
    Nothing -> return ()
  

onClick :: World -> [(Tile, Bitmap)] ->
           IORef State -> MouseData -> IO ()
onClick world _ stateRef (MouseData mousePos _ _) = do
  (mode, mapState) <- readIORef stateRef 
  case mode of
    Normal -> movePlayer world mapState stateRef mousePos
    Locked -> return ()

play :: Maybe String -> IO ()
play (Just worldStr) = do 
  case parseWorld worldStr of
    Just world -> do
      Just ce <- elemById "canvas"
      imgs <- loadImages world
      let sMap = startMap world
      let sPoint = startPoint sMap
      let sPicture = drawMap imgs sMap

      stateRef <- newIORef (Normal, (sMap, sPoint, sPicture))
      onEvent ce Click $ onClick world imgs stateRef
      renderState sPicture sPoint
      eventPoint sMap world sPoint stateRef
    Nothing -> alert "Map parsing error"
play Nothing = alert "Map file not loaded"

changeOutputHTML :: String -> IO ()
changeOutputHTML s = do
  Just e <- elemById "output"
  setProp e "innerHTML" s
