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

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

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
  | abs xT > abs yT = (updateCoord (block / 2) xT xS, yS)
  | otherwise = (xS, updateCoord (block / 2) yT yS)
  where
    xT = fromIntegral x - width / 2
    yT = fromIntegral y - height / 2

interPoints :: Double -> Point -> Point -> [Point]
interPoints l (x1, y1) (x2, y2) = (take (floor l) $ zip
                                                (iterate (+xdiff) x1)
                                                (iterate (+ydiff) y1))
                                                  ++ [(x2, y2)]
  where
    xdiff = (x2 - x1) / l
    ydiff = (y2 - y1) / l

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

fades :: Double
fades = 10
fadeOut, fadeIn :: [Double]
fadeOut = map (/fades) [0..fades]
fadeIn = tail $ reverse fadeOut

---------------------------------Impure-------------------------------

animateMove :: IORef State -> (Point -> IO ()) ->
               [Point] -> IO () -> IO ()
animateMove _ _ [] finalAction = finalAction
animateMove stateRef renderState' (nextPoint:xs) fA = do
  (mode, _, _, _) <- readIORef stateRef
  case mode of
    NoEvent -> animateMove stateRef renderState' [] fA
    _ -> do
      renderState' nextPoint
      setTimer (Once 10) (animateMove stateRef renderState' xs fA)
      return ()

animateFades :: IORef State -> (Double -> Picture ()) ->
                [(IO (), [Double])] ->
                IO () -> IO ()
animateFades _ _ [] finalAction = finalAction
animateFades stateRef fader ((_, []):xRS) fA =
                                     animateFades stateRef fader xRS fA
animateFades stateRef fader ((renderState', (fadeLevel:xFL)):xRS) fA = do
  (mode, _, _, _) <- readIORef stateRef
  case mode of
    NoEvent -> animateFades stateRef fader [] fA
    _ -> do 
      renderState'
      renderStateOnTop (fader fadeLevel) (0, 0)
      setTimer (Once 20) (animateFades stateRef fader
                                       ((renderState', xFL):xRS)
                                       fA)
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
  renderStateOnTop (drawText (s1, s2)) (0, 0)
  writeIORef stateRef (Text rest2, m, world, imgs)
    where
      (s1, rest1) = parseDrawText s
      (s2, rest2) = parseDrawText rest1

eventPoint' (FullText "" "") stateRef = do
  (_, (m, p, picture, pImg), world, imgs) <- readIORef stateRef
  writeIORef stateRef (NoEvent, (m, p, picture, pImg), world, imgs)
eventPoint' (FullText h s) stateRef = do
  (_, (m, p, picture, pImg), world, imgs) <- readIORef stateRef
  writeIORef stateRef (FullText "" "",
                       (m, p, picture, pImg),
                       world, imgs)
  renderStateOnTop fullText (0, 0)
  setTimer (Once 2000) $ animateMove stateRef
    (renderStateOnTop fullText)
    (interPoints 1000 p1 p2)
    (do
       (_, (m', p', picture', pImg'), world', imgs') <-
                                                     readIORef stateRef
       writeIORef stateRef (Locked, (m', p', picture', pImg'),
                            world', imgs')
       animateFades stateRef fullBlack [(renderState pImg' picture' p',
                                         fadeIn)]
                                       (do
       (_, (m'', p'', picture'', pImg''), world'', imgs'') <-
                                                     readIORef stateRef
       writeIORef stateRef (NoEvent, (m'', p'', picture'', pImg''),
                            world'', imgs'')))
  return ()             
    where
      (p1, p2, fullText) = drawFullText h sDraw
      sDraw = map fst sParsed
      sParsed = takeWhileOneMore ((/="") . snd) $
                                 iterate (parseDrawText . snd) $
                                 parseDrawText s

eventPoint' (HTMLText s) _ = changeOutputHTML s

eventPoint' (Teleport m p') stateRef = do
  (_, (_, p, picture, pImg), world, imgs) <- readIORef stateRef
  let Just (MapContent newMap) = lookup m world
  let newPicture = drawMap imgs newMap
  writeIORef stateRef (Locked,
                       (newMap, p', newPicture, pImg),
                       world, imgs)
  animateFades stateRef fullBlack
               [(renderState pImg picture p, fadeOut),
               (renderState pImg newPicture p', fadeIn)]
               (do
    (_, (m', p'', picture', pImgs'), world', imgs') <-
                                                     readIORef stateRef
    writeIORef stateRef (NoEvent, (m', p'', picture', pImgs'),
                         world', imgs'))
  return ()

eventPoint' _ _ = return ()


eventPoint :: IORef State -> IO ()
eventPoint stateRef = do
  (_, ((c, tiles), (x, y), _, _), world, _) <- readIORef stateRef
  let tile = tiles !! floor (x + c * y)
  let Just (TileItem _ e) = lookup tile world
  eventPoint' e stateRef
-----------------------------------------------------------------------

movePlayer :: IORef State -> (Int, Int) -> IO ()
movePlayer stateRef mousePos = do
  (_, (m, p, picture, pImg), world, imgs) <- readIORef stateRef
  changeOutputHTML ""
  case validPoint m (updatePoint mousePos p) of
    Just p' -> do 
      writeIORef stateRef (Locked, (m, p', picture, pImg), world, imgs)
      animateMove stateRef
                  (renderState pImg picture)
                  (interPoints 5 p p')
                  (do
        (_, (m', p'', picture', pImg'), world', imgs') <-
                                                     readIORef stateRef
        writeIORef stateRef (NoEvent, (m', p'', picture', pImg'),
                             world', imgs')
        eventPoint stateRef)
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
