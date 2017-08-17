module Game (play) where

import World
import Graphics

import Haste
import Haste.DOM
import Haste.Events

import Data.IORef
import Data.List

type MapState = (MapContent', Point, Picture (), Maybe Bitmap)
type State = (EventItem, MapState, World, Imgs)

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
  | abs xT > abs yT = (updateCoord (width / blocks / 2) xT xS, yS)
  | otherwise = (xS, updateCoord (height / blocks / 2) yT yS)
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
  (_, (m, p, picture, pImgs), world, imgs) <- readIORef stateRef
  writeIORef stateRef (NoEvent, (m, p, picture, pImgs), world, imgs)
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
eventPoint' :: EventItem -> IORef State -> IO ()
eventPoint' (EventItemList []) _ = return ()
eventPoint' (EventItemList (nextEvent:xs)) stateRef = do
  (event, _, _, _) <- readIORef stateRef
  case event of
    NoEvent -> do 
      eventPoint' nextEvent stateRef
      eventPoint' (EventItemList xs) stateRef
    _ -> do
      setTimer (Once 100) $ eventPoint' (EventItemList (nextEvent:xs))
                                        stateRef
      return ()

eventPoint' (Text "") stateRef = do
  (_, (m, p, picture, pImg), world, imgs) <- readIORef stateRef
  writeIORef stateRef (NoEvent, (m, p, picture, pImg), world, imgs)
  renderState pImg picture p
eventPoint' (Text s) stateRef = do
  (_, m, world, imgs) <- readIORef stateRef
  renderStateOnTop $ drawText (s1, s2)
  writeIORef stateRef (Text rest2, m, world, imgs)
    where
      (s1, rest1) = parseDrawText s
      (s2, rest2) = parseDrawText rest1

eventPoint' (FullText "" "") stateRef = do
  (_, (m, p, picture, pImg), world, imgs) <- readIORef stateRef
  writeIORef stateRef (NoEvent, (m, p, picture, pImg), world, imgs)
  renderState pImg picture p
eventPoint' (FullText h s) stateRef = do
  (_, (m, p, picture, pImg), world, imgs) <- readIORef stateRef
  renderState pImg picture p
  renderStateOnTop $ drawFullText h [s1, s2, s3, s4, s5]
  writeIORef stateRef (FullText "" rest5, (m, p, picture, pImg), world, imgs)
    where
      (s1, rest1) = parseDrawText s
      (s2, rest2) = parseDrawText rest1
      (s3, rest3) = parseDrawText rest2
      (s4, rest4) = parseDrawText rest3
      (s5, rest5) = parseDrawText rest4

eventPoint' (HTMLText s) _ = changeOutputHTML s

eventPoint' (Teleport m p') stateRef = do
  (_, (_, p, picture, pImg), world, imgs) <- readIORef stateRef
  let Just (MapContent newMap) = lookup m world
  let newPicture = drawMap imgs newMap
  let fades = 10
  let fadeOut = map (/fades) [0..fades]
  let fadeIn = tail $ reverse fadeOut
  writeIORef stateRef (Locked, (newMap, p', newPicture, pImg), world, imgs)
  animateFades fullBlack [(renderState pImg picture p, fadeOut),
                     (renderState pImg newPicture p', fadeIn)] stateRef
  return ()

eventPoint' _ _ = return ()


eventPoint :: IORef State -> IO ()
eventPoint stateRef = do
  (_, ((c, tiles), (x, y), _, _), world, _) <- readIORef stateRef
  let tile = tiles !! floor (x + c * y)
  let Just (TileItem _ e) = lookup tile world
  eventPoint' e stateRef
-----------------------------------------------------------------------

animateMovePlayer :: IORef State -> (Point -> IO ()) ->
                     [Point] -> IO ()
animateMovePlayer stateRef _ []  = do
  (_, (m, p, picture, pImg), world, imgs) <- readIORef stateRef
  writeIORef stateRef (NoEvent, (m, p, picture, pImg), world, imgs)
  eventPoint stateRef
  return ()
animateMovePlayer stateRef renderState' (nextState:xs)  = do
  renderState' nextState
  setTimer (Once 10) (animateMovePlayer stateRef renderState' xs)
  return ()

movePlayer :: IORef State -> (Int, Int) -> IO ()
movePlayer stateRef mousePos = do
  (_, (m, p, picture, pImg), world, imgs) <- readIORef stateRef
  changeOutputHTML ""
  case validPoint m (updatePoint mousePos p) of
    Just p' -> do 
      writeIORef stateRef (Locked, (m, p', picture, pImg), world, imgs)
      animateMovePlayer stateRef
                        (renderState pImg picture) (interPoints p p')
    Nothing -> return ()
  

onClick :: IORef State -> MouseData -> IO ()
onClick stateRef (MouseData mousePos _ _) = do
  (event, _, _, _) <- readIORef stateRef 
  case event of
    NoEvent -> movePlayer stateRef mousePos
    _ -> eventPoint' event stateRef

play :: Maybe String -> IO ()
play (Just worldStr) = do 
  case parseWorld worldStr of
    Just world -> do
      Just ce <- elemById "canvas"
      imgs <- loadImages world
      let sMap = startMap world
      let sPoint = startPoint sMap
      let sPicture = drawMap imgs sMap
      let pImg = lookup (Player 1) imgs

      stateRef <- newIORef (NoEvent, (sMap, sPoint, sPicture, pImg),
                            world, imgs)
      onEvent ce Click $ onClick stateRef
      renderState pImg sPicture sPoint
      eventPoint stateRef
    Nothing -> alert "Map parsing error"
play Nothing = alert "Map file not loaded"

changeOutputHTML :: String -> IO ()
changeOutputHTML s = do
  Just e <- elemById "output"
  setProp e "innerHTML" s
