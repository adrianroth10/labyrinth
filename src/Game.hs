module Game (play) where

import World
import Graphics
 
import Haste
import Haste.DOM
import Haste.Events

import Data.IORef
import Data.List

type MapState = (MapContent', Point, Point -> IO ())
type State = (EventItem, MapState, World, Imgs)

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

flippedLookup :: Eq a => a -> [(b, a)] -> Maybe b
flippedLookup m = lookup m . uncurry (flip zip) . unzip 

-------------------------------Init------------------------------------
startMap :: World -> MapContent'
startMap [] = error "No start found"
startMap ((_, MapContent (c, tiles)):xti)
  | elem Start tiles = (c, tiles)
  | otherwise = startMap xti
startMap (_:xti) = startMap xti

startPoint :: MapContent' -> Point
startPoint (c, tiles) = (x, y)
  where
    (Just i) = elemIndex Start tiles
    x = fromIntegral (mod i (floor c))
    y = fromInteger (floor ((realToFrac i) / c))
-----------------------------------------------------------------------

-------------------------MovePlayer------------------------------------
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
-----------------------------------------------------------------------

-------------------------Animation------------------------------------
fades :: Double
fades = 10
fadeOut, fadeIn :: [Double]
fadeOut = map (/fades) [0..fades]
fadeIn = tail $ reverse fadeOut

interPoints :: Double -> Point -> Point -> [Point]
interPoints l (x1, y1) (x2, y2) = (take (floor l) $ zip
                                                (iterate (+xdiff) x1)
                                                (iterate (+ydiff) y1))
                                                  ++ [(x2, y2)]
  where
    xdiff = (x2 - x1) / l
    ydiff = (y2 - y1) / l
-----------------------------------------------------------------------

------------------------------Fight------------------------------------
drawPlayers' :: (Tile, Tile) -> World -> Imgs ->
                (Picture (), Picture ())
drawPlayers' (player1, player2) world imgs =
                  drawPlayers (pBit1, pBit2) (playerItem1, playerItem2)
  where
     Just playerItem1 = lookup player1 world
     Just playerItem2 = lookup player2 world
     Just pBit1 = lookup player1 imgs
     Just pBit2 = lookup player1 imgs

startHp :: Point
startHp = (100, 100)

getIndex :: (Int, Int) -> Int
getIndex (x, y)
  | xT < 0 && yT < 0 = 0
  | xT > 0 && yT < 0 = 1
  | xT < 0 && yT > 0 = 2
  | otherwise = 3
    where
      xT = fromIntegral x - width / 2
      yT = fromIntegral y - height / 2

extract :: [a] -> Int -> a
extract [] _ = error "Not supposed to happen"
extract [x] _ = x
extract (x:_) 0 = x
extract (_:xs) i = extract xs (i - 1)

-- Creates an almost random number in the interval [0, h)
pseudoRandom :: Int -> (Int, Int) -> Int
pseudoRandom h (x, y) = mod (x + y) h

fight :: World -> (Tile, Tile) -> Point -> (Int, Int) ->
         (EventItem, EventItem) -> (Point, EventItem)
fight world (player1, player2) hp mousePos end
  | hp2 <= 0 = ((hp1, 0), fst end)
  | hp1 <= 0 = ((0, hp2), snd end)
  | otherwise = ((hp1, hp2),
                EventItemList [Text (name1 ++ " used " ++ attack1), e1,
                               Text (name2 ++ " used " ++ attack2), e2,
                               Fight Locked (player1, player2) end])
  where
    Just (PlayerItem _ name1 moves1) = lookup player1 world
    Just (PlayerItem _ name2 moves2) = lookup player2 world
    (attack1, damage1, e1) = extract moves1 (getIndex mousePos)
    (attack2, damage2, e2) = extract moves2
                                     (pseudoRandom (length moves2)
                                                   mousePos)
    (hp1, hp2) = hp <+> (-damage2, -damage1)
-----------------------------------------------------------------------


-----------------------------------------------------------------------
---------------------------------Impure-------------------------------
-----------------------------------------------------------------------

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
event :: EventItem -> IORef State -> IO ()
event (Text "") stateRef = do
  (_, (m, p, render), world, imgs) <- readIORef stateRef
  writeIORef stateRef (NoEvent, (m, p, render), world, imgs)
  render p
event (Text s) stateRef = do
  (_, m, world, imgs) <- readIORef stateRef
  renderStateOnTop (drawText (s1, s2)) (0, 0)
  writeIORef stateRef (Text rest2, m, world, imgs)
    where
      (s1, rest1) = parseDrawText s
      (s2, rest2) = parseDrawText rest1

event (FullText "" "") stateRef = do
  (_, m, world, imgs) <- readIORef stateRef
  writeIORef stateRef (NoEvent, m, world, imgs)
event (FullText h s) stateRef = do
  (_, (m, p, render), world, imgs) <- readIORef stateRef
  writeIORef stateRef (FullText "" "", (m, p, render),
                       world, imgs)
  renderStateOnTop fullText (0, 0)
  setTimer (Once 2000) $ animateMove stateRef
    (renderStateOnTop fullText)
    (interPoints 1000 p1 p2)
    (do
       (_, m', world', imgs') <- readIORef stateRef
       writeIORef stateRef (Locked, m', world', imgs')
       animateFades stateRef fullBlack [(render p, fadeIn)]
                                       (do
       (_, m'', world'', imgs'') <- readIORef stateRef
       writeIORef stateRef (NoEvent, m'', world'', imgs'')))
  return ()             
    where
      (p1, p2, fullText) = drawFullText h sDraw
      sDraw = map fst sParsed
      sParsed = takeWhileOneMore ((/="") . snd) $
                                 iterate (parseDrawText . snd) $
                                 parseDrawText s

event (HTMLText s) _ = changeOutputHTML s

event (Teleport m p') stateRef = do
  (_, (_, p, render), world, imgs) <- readIORef stateRef
  let Just (MapContent newMap) = lookup m world
  let newPicture = drawMap imgs newMap
  let pImg = lookup (Player 1) imgs
  writeIORef stateRef (Locked,
                       (newMap, p', renderState pImg newPicture),
                       world, imgs)
  animateFades stateRef fullBlack
               [(render p, fadeOut),
                (renderState pImg newPicture p', fadeIn)]
               (do
    (_, m', world', imgs') <- readIORef stateRef
    writeIORef stateRef (NoEvent, m', world', imgs'))

event (Fight Locked players end) stateRef = do
  (_, m, world, imgs) <- readIORef stateRef
  writeIORef stateRef (Fight Locked players end,
                       m, world, imgs)
event (Fight e1 (player1, player2) (win, lose)) stateRef = do
  (_, (m, p, _), world, imgs) <- readIORef stateRef
  let Just mapTile = flippedLookup (MapContent m) world
  let teleport = Teleport mapTile p

  let Just (PlayerItem _ _ moves) = lookup player1 world
  let (pImg1, pImg2) = drawPlayers' (player1, player2) world imgs
  fightImage <- return $ do
    mergePlayerPictures (pImg1, pImg2)
    drawMoves moves
  let render = flip renderStateOnTop (0, 0) . updatePlayers fightImage

  render startHp
  writeIORef stateRef (NoEvent, (m, startHp, render), world, imgs)
  event (EventItemList [e1, Fight Locked (player1, player2)
                                  (EventItemList [win, teleport],
                                   EventItemList [lose, teleport])])
        stateRef

event (EventItemList []) _ = return ()
event (EventItemList (EventItemList l:xs)) stateRef = 
                               event (EventItemList (l ++ xs)) stateRef
event (EventItemList (nextEvent:xs)) stateRef = do
  (event', _, _, _) <- readIORef stateRef
  case event' of
    NoEvent -> do 
      event nextEvent stateRef
      event (EventItemList xs) stateRef
    _ -> do
      setTimer (Once 100) $ event (EventItemList (nextEvent:xs))
                                        stateRef
      return ()

event _ _ = return ()


eventPoint :: IORef State -> IO ()
eventPoint stateRef = do
  (_, ((c, tiles), (x, y), _), world, _) <- readIORef stateRef
  let tile = tiles !! floor (x + c * y)
  let Just (TileItem _ e) = lookup tile world
  event e stateRef
-----------------------------------------------------------------------

playerMove :: IORef State -> (Int, Int) -> IO ()
playerMove stateRef mousePos = do
  (_, (m, p, render), world, imgs) <- readIORef stateRef
  changeOutputHTML ""
  case validPoint m (updatePoint mousePos p) of
    Just p' -> do 
      writeIORef stateRef (Locked, (m, p', render), world, imgs)
      animateMove stateRef
                  (render)
                  (interPoints 5 p p')
                  (do
        (_, m', world', imgs') <- readIORef stateRef
        writeIORef stateRef (NoEvent, m', world', imgs')
        eventPoint stateRef)
    Nothing -> return ()

fightMove :: EventItem -> IORef State -> (Int, Int) -> IO ()
fightMove (Fight _ players end) stateRef mousePos = do
  (_, (m, hp, render), world, imgs) <- readIORef stateRef
  let (newHp, event') = fight world players hp mousePos end
  render newHp
  writeIORef stateRef (NoEvent,
                       (m, newHp, render), world, imgs)
  event event' stateRef
fightMove _ _ _ = return ()

onClick :: IORef State -> MouseData -> IO ()
onClick stateRef (MouseData mousePos _ _) = do
  (event', _, _, _) <- readIORef stateRef 
  case event' of
    NoEvent -> playerMove stateRef mousePos
    Fight _ _ _ -> fightMove event' stateRef mousePos
    _ -> event event' stateRef

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

      stateRef <- newIORef (NoEvent,
                            (sMap, sPoint, renderState pImg sPicture),
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
