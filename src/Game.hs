module Game (play) where

import World
import Graphics

import Haste
import Haste.DOM
import Haste.Events

import Data.IORef
import Data.List

type MapState = (MapContent', Point, Picture ())
data Mode = Normal | TextEvent String | Locked deriving Eq
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

animateFades :: (Double -> Picture ()) ->
                [(IO (), [Double])] ->
                IORef State -> IO ()
animateFades _ [] stateRef = do
  (_, (m, p, mPicture)) <- readIORef stateRef
  writeIORef stateRef (Normal, (m, p, mPicture))
  return ()
animateFades fader ((_, []):xRS) stateRef =
                                        animateFades fader xRS stateRef
animateFades fader ((renderState', (fadeLevel:xFL)):xRS) stateRef = do
    renderState'
    renderStateOnTop $ fader fadeLevel
    setTimer (Once 20) (animateFades fader
                                     ((renderState', xFL):xRS)
                                     stateRef)
    return ()

-------------------------------Events----------------------------------
eventPoint' :: World -> Imgs -> EventItem -> IORef State -> IO ()
eventPoint' _ _ (EventItemList _) _ = return ()

eventPoint' _ _ (Text "") stateRef = do
  (_, (m, p, picture)) <- readIORef stateRef
  writeIORef stateRef (Normal, (m, p, picture))
  renderState picture p
eventPoint' _ _ (Text s) stateRef = do
  (_, m) <- readIORef stateRef
  renderStateOnTop $ drawText s
  writeIORef stateRef (TextEvent s', m)
    where s' = restText $ restText s

eventPoint' _ _ (HTMLText s) _ = changeOutputHTML s

eventPoint' world imgs (Teleport m p') stateRef = do
  (_, (_, p, picture)) <- readIORef stateRef
  writeIORef stateRef (Locked, (newMap, p', newPicture))
  animateFades fullBlack [(renderState picture p, fadeOut),
                          (renderState newPicture p', fadeIn)] stateRef
  return ()
    where
      Just (MapContent newMap) = lookup m world
      newPicture = drawMap imgs newMap
      fades = 10
      fadeOut = map (/fades) [0..fades]
      fadeIn = tail $ reverse fadeOut

eventPoint' _ _ NoEvent _ = return ()


eventPoint :: World -> Imgs -> MapContent' -> Point -> IORef State -> IO ()
eventPoint world imgs (c, tiles) (x, y) stateRef =
                                        eventPoint' world imgs e stateRef
  where
    tile = tiles !! floor (x + c * y)
    Just (TileItem _ e) = lookup tile world
-----------------------------------------------------------------------

animateMovePlayer :: World -> Imgs -> IORef State -> (Point -> IO ()) ->
                     [Point] -> IO ()
animateMovePlayer world imgs stateRef _ []  = do
  (_, (m, p, mPicture)) <- readIORef stateRef
  writeIORef stateRef (Normal, (m, p, mPicture))
  eventPoint world imgs m p stateRef
  return ()
animateMovePlayer world imgs stateRef renderState' (nextState:xs)  = do
  renderState' nextState
  setTimer (Once 10) (animateMovePlayer world imgs stateRef renderState' xs)
  return ()

movePlayer :: World -> Imgs -> MapState -> IORef State -> (Int, Int) -> IO ()
movePlayer world imgs (m, p, mPicture) stateRef mousePos = do
  case validPoint m (updatePoint mousePos p) of
    Just p' -> do 
      writeIORef stateRef (Locked, (m, p', mPicture))
      animateMovePlayer world imgs stateRef
                        (renderState mPicture) (interPoints p p')
    Nothing -> return ()
  

onClick :: World -> Imgs -> IORef State -> MouseData -> IO ()
onClick world imgs stateRef (MouseData mousePos _ _) = do
  (mode, mapState) <- readIORef stateRef 
  changeOutputHTML ""
  case mode of
    Normal -> movePlayer world imgs mapState stateRef mousePos
    TextEvent s -> eventPoint' world imgs (Text s) stateRef
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
      eventPoint world imgs sMap sPoint stateRef
    Nothing -> alert "Map parsing error"
play Nothing = alert "Map file not loaded"

changeOutputHTML :: String -> IO ()
changeOutputHTML s = do
  Just e <- elemById "output"
  setProp e "innerHTML" s
