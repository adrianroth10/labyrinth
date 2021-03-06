module Game (play) where

import World
import Graphics

import Haste
import Haste.DOM
import Haste.Events

import Data.IORef
import Data.List
import Data.Maybe
import Control.Arrow

type MapState = (MapItem', Point, Point -> IO ())
type State = (EventItem, MapState, World, Imgs, Tile)

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

flippedLookup :: Eq a => a -> [(b, a)] -> Maybe b
flippedLookup m = lookup m . uncurry (flip zip) . unzip

replaceLookups :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
replaceLookups xs w = foldl (\ acc x -> x : filter ((fst x /=) . fst) acc) w xs


-------------------------------Init------------------------------------
startMap :: World -> MapItem'
startMap [] = error "No start found"
startMap ((_, MapItem (c, tiles)):xti)
  | Start `elem` tiles = (c, tiles)
  | otherwise = startMap xti
startMap (_:xti) = startMap xti

startPoint :: MapItem' -> Point
startPoint (c, tiles) = (x, y)
  where
    (Just i) = elemIndex Start tiles
    x = fromIntegral (mod i (floor c))
    y = fromInteger (floor (realToFrac i / c))
-----------------------------------------------------------------------

-------------------------MovePlayer------------------------------------
validPoint :: MapItem' -> Point -> Maybe Point
validPoint (c, tiles) (x, y)
  | x < 0 || y < 0 || x >= c || i >= length tiles = Nothing
  | eqTile (tiles !! i) (Wall 1) = Nothing
  | otherwise = Just (x, y)
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

eventPoint :: World -> MapItem' -> Point -> EventItem
eventPoint world (c, tiles) (x, y) = e
  where
    tile = tiles !! floor (x + c * y)
    Just (TileItem _ e) = lookup tile world
-----------------------------------------------------------------------

-------------------------Animation------------------------------------
fades :: Double
fades = 10
fadeOut, fadeIn :: [Point]
fadeOut = map (\f -> (f/fades, 0)) [0..fades]
fadeIn = tail $ reverse fadeOut

faderHelper :: Point -> (Point -> IO ()) -> (Double -> Picture ()) ->
                   Point -> IO ()
faderHelper point render fader (f, _) = do
  render point
  renderStateOnTop (fader f) (0, 0)

animateHelper :: [(Point -> IO (), [Point])] -> EventItem -> EventItem
animateHelper = (Animation .) . AnimationInfo

fTInterPoints :: [String] -> Double
fTInterPoints = fromIntegral . (100*) . length

interPoints :: Double -> Point -> Point -> [Point]
interPoints l (x1, y1) (x2, y2) = take (floor l) (zip
                                                (iterate (+xdiff) x1)
                                                (iterate (+ydiff) y1))
                                                  ++ [(x2, y2)]
  where
    xdiff = (x2 - x1) / l
    ydiff = (y2 - y1) / l
-----------------------------------------------------------------------

------------------------------Fight------------------------------------
drawPlayers' :: (Tile, Tile) -> World -> Imgs ->
                ((Point, Picture ()), (Point, Picture ()))
drawPlayers' (player1, player2) world imgs =
                  drawPlayers (pBit1, pBit2) (playerItem1, playerItem2)
  where
     Just playerItem1 = lookup player1 world
     Just playerItem2 = lookup player2 world
     Just pBit1 = lookup player1 imgs
     Just pBit2 = lookup player2 imgs

startHp :: Point
startHp = (100, 100)

psychadelic :: [Point]
psychadelic = foldl (\acc p -> acc ++ replicate 5 p) [] $
                    intersperse (0, 0) $ take 5 fadeIn ++
                                         drop 5 fadeOut

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

fight :: World -> (Tile, Tile) -> Point -> (Point -> IO ()) ->
         (Int, Int) -> (EventItem, EventItem) -> EventItem
fight world (player1, player2) hp render mousePos end
  | hp2 <= 0 = EventItemList [animation1, fst end]
  | hp1 <= 0 = EventItemList [animation1, animation2, snd end]
  | otherwise = EventItemList [animation1, animation2,
                               Fight Locked (player1, player2) end]
  where
    Just (PlayerItem _ name1 moves1) = lookup player1 world
    Just (PlayerItem _ name2 moves2) = lookup player2 world
    (attack1, damage1, e1) = extract moves1 (getIndex mousePos)
    (attack2, damage2, e2) = extract moves2
                                     (pseudoRandom (length moves2)
                                                   mousePos)
    newDamage1 = min damage1 (snd hp)
    newDamage2 = min damage2 (fst hp)
    hpI = hp |+| (0, -newDamage1)
    (hp1, hp2) = hpI |+| (-newDamage2, 0)

    animation1 = EventItemList [Text (name1 ++ " used " ++ attack1),
                   e1,
                   animateHelper [(render,
                       interPoints (abs newDamage1) hp hpI)] Locked,
                   ChangePoint hpI]
    animation2 = EventItemList [Text (name2 ++ " used " ++ attack2),
                   e2,
                   animateHelper [(render,
                       interPoints (abs newDamage2) hpI (hp1, hp2))] Locked,
                   ChangePoint (hp1, hp2)]

-----------------------------------------------------------------------
--
-----------------------------Checkpoint--------------------------------
rFirstEventCheckpoint :: [EventItem] -> [EventItem]
rFirstEventCheckpoint [] = []
rFirstEventCheckpoint (IncrementCheckpoints _ _:es) = es
rFirstEventCheckpoint (e:es) = e : rFirstEventCheckpoint es

rEventCheckpoint :: EventItem -> EventItem
rEventCheckpoint (IncrementCheckpoints _ _) = NoEvent
rEventCheckpoint (EventItemList es)  = EventItemList $
                                               rFirstEventCheckpoint es
rEventCheckpoint e = e

rMovesCheckpoint :: Moves -> Moves
rMovesCheckpoint = map (\(a, b, c) -> (a, b, rEventCheckpoint c))

removeCheckpoint :: Tile -> World -> World
removeCheckpoint NoTile w = w
removeCheckpoint (Map _) w = w
removeCheckpoint (Player n) w = replaceLookups
        [(Player n, PlayerItem img name (rMovesCheckpoint ms))] w
    where Just (PlayerItem img name ms) = lookup (Player n) w
removeCheckpoint t w = replaceLookups
        [(t, TileItem img (rEventCheckpoint es))] w
    where Just (TileItem img es) = lookup t w



-----------------------------------------------------------------------
---------------------------------Impure--------------------------------
-----------------------------------------------------------------------

animation :: IORef State -> [(Point -> IO (), [Point])] -> IO ()
animation stateRef [] = do
  (EventItemList (_ : eis), m, world, imgs, check) <- readIORef stateRef
  writeIORef stateRef (NoEvent, m, world, imgs, check)
  event stateRef $ EventItemList eis
animation stateRef renderList = do
  (mode, _, _, _, _) <- readIORef stateRef
  case mode of
    EventItemList (NoEvent : _) -> do
      mapM_ (\(render, nextPoint:_) -> render nextPoint) lastPoints
      animation stateRef []
    _ -> do
      mapM_ (\(render, nextPoint:_) -> render nextPoint) renderList
      _ <- setTimer (Once 10) (animation stateRef tailPoints)
      return ()
    where
      tailPoints' = map (second tail) renderList
      tailPoints = filter (\(_, l) -> not (null l)) tailPoints'
      lastPoints = map (\(render, pointList) ->
                        (render, [last pointList])) renderList

changeCheckpoint :: IORef State -> [EventItem] -> World -> Tile ->
                    (Integer -> Integer) -> IO ()
changeCheckpoint stateRef eis w t f = do
  (_, (m, point, render), world, _, Checkpoint n) <- readIORef stateRef
  let CheckpointItem newWorldItems es = fromMaybe (CheckpointItem [] NoEvent) $
                                      lookup (Checkpoint (f n)) world
  let Just mapTile = flippedLookup (MapItem m) world
  let newWorld = replaceLookups newWorldItems $ replaceLookups w $
                    removeCheckpoint t world
  imgs <- loadImages newWorld
  writeIORef stateRef (NoEvent, (m, point, render),
                       newWorld, imgs, Checkpoint (f n))
  event stateRef $ EventItemList $ [Teleport mapTile point, es] ++ eis

-------------------------------Events----------------------------------
event :: IORef State -> EventItem ->  IO ()
event _ (EventItemList []) = return ()

event stateRef (EventItemList (Text "" : eis)) = do
  (_, (m, p, render), world, imgs, check) <- readIORef stateRef
  writeIORef stateRef (NoEvent, (m, p, render), world, imgs, check)
  render p
  event stateRef $ EventItemList eis
event stateRef (EventItemList (Text s : eis)) = do
  (_, m, world, imgs, check) <- readIORef stateRef
  renderStateOnTop (drawText (s1, s2)) (0, 0)
  writeIORef stateRef (EventItemList (Text rest : eis),
                       m, world, imgs, check)
    where
      (s1, rest1) = parseDrawText s
      (s2, rest2) = parseDrawText rest1
      rest = case rest2 of
               "" -> ""
               _  -> rest1

event stateRef (EventItemList (FullText "" "" : eis)) = do
  (_, m, world, imgs, check) <- readIORef stateRef
  writeIORef stateRef (EventItemList (NoEvent : eis), m, world, imgs, check)
event stateRef (EventItemList (FullText h s : eis)) = do
  (_, (_, p, render), _, _, _) <- readIORef stateRef
  let animation1 = animateHelper [(renderStateOnTop fullText,
                                   interPoints (fTInterPoints sDraw) p1 p2)]
                                 (FullText "" "")
  let animation2 = animateHelper [(faderHelper p render fullBlack,
                                   fadeIn)]
                                 (FullText "" "")
  renderStateOnTop fullText (0, 0)
  _ <- setTimer (Once 3000) $
        event stateRef $ EventItemList $ [animation1, animation2] ++ eis
  return ()
    where
      (p1, p2, fullText) = drawFullText h sDraw
      sDraw = map fst sParsed
      sParsed = takeWhileOneMore ((/="") . snd) $
                                 iterate (parseDrawText . snd) $
                                 parseDrawText s

event stateRef (EventItemList (HTMLText s : eis)) = do
  changeOutputHTML s
  event stateRef $ EventItemList eis

event stateRef (EventItemList (Teleport m p' : eis)) = do
  (_, (_, p, render), world, imgs, check) <- readIORef stateRef
  let Just (MapItem newMap) = lookup m world
  let newPicture = drawMap imgs newMap
  let pImg = lookup (Player 1) imgs
  writeIORef stateRef (NoEvent,
                       (newMap, p', renderState pImg newPicture),
                       world, imgs, check)
  event stateRef $ EventItemList $
    [animateHelper [(faderHelper p render fullBlack, fadeOut)] Locked,
     animateHelper [(faderHelper p' (renderState pImg newPicture)
                                 fullBlack, fadeIn)] Locked] ++ eis

event stateRef (EventItemList (Fight Locked players end : eis)) = do
  (_, m, world, imgs, check) <- readIORef stateRef
  writeIORef stateRef (EventItemList (Fight Locked players end : eis),
                       m, world, imgs, check)
event stateRef (EventItemList (Fight e1 (player1, player2) (win, lose) : eis)) = do
  (_, (m, p, lastRender), world, imgs, check) <- readIORef stateRef
  let Just mapTile = flippedLookup (MapItem m) world
  let teleport = Teleport mapTile p

  let Just (PlayerItem _ _ moves) = lookup player1 world
  let ((p1, pImg1), (p2, pImg2)) = drawPlayers' (player1, player2)
                                                world imgs
  let fightImage = mergePlayerPictures (pImg1, pImg2) moves
  let render = flip renderStateOnTop (0, 0) . updatePlayers fightImage

  render startHp
  writeIORef stateRef (NoEvent, (m, startHp, render), world, imgs, check)
  event stateRef $ EventItemList $
    [animateHelper [(faderHelper p lastRender fullWhite,
                     psychadelic)] Locked,
     animateHelper [(renderStateOnTop pImg1,
                     interPoints 200 (p1 |+| (width, 0)) p1),
                    (renderStateOnTop pImg2,
                     interPoints 200 (p2 |+| (-width, 0)) p2)] Locked,
     e1, Fight Locked (player1, player2)
               (EventItemList [win, teleport],
                EventItemList [lose, teleport])] ++ eis

event stateRef (EventItemList (IncrementCheckpoints t w : eis)) =
                                      changeCheckpoint stateRef eis w t (+1)

event stateRef (EventItemList (SetCheckpoint n : eis)) =
                                      changeCheckpoint stateRef eis [] NoTile (const n)

event stateRef (EventItemList (Animation (AnimationInfo renderList event') : eis)) = do
  (_, m, world, imgs, check) <- readIORef stateRef
  writeIORef stateRef (EventItemList (event': eis), m, world, imgs, check)
  animation stateRef renderList

event stateRef (EventItemList (ChangePoint point : eis)) = do
  (event', (m, _, render), world, imgs, check) <- readIORef stateRef
  writeIORef stateRef (event', (m, point, render), world, imgs, check)
  event stateRef $ EventItemList eis

event stateRef (EventItemList (EventItemList l:xs)) =
                               event stateRef $ EventItemList $ l ++ xs

event _ _ = return ()


-----------------------------------------------------------------------

playerMove :: IORef State -> (Int, Int) -> IO ()
playerMove stateRef mousePos = do
  (_, (m, p, render), world, imgs, check) <- readIORef stateRef
  changeOutputHTML ""
  case validPoint m (updatePoint mousePos p) of
    Just p' -> do
      writeIORef stateRef (NoEvent, (m, p', render), world, imgs, check)
      event stateRef $
            EventItemList [animateHelper [(render, interPoints 5 p p')]
                                     Locked,
                           eventPoint world m p']
    Nothing -> return ()

fightMove :: IORef State -> (Int, Int) -> IO ()
fightMove stateRef mousePos = do
  (EventItemList (Fight _ players end : eis),
   (m, hp, render), world, imgs, check) <- readIORef stateRef
  let event' = fight world players hp render mousePos end
  writeIORef stateRef (NoEvent, (m, hp, render), world, imgs, check)
  event stateRef $ EventItemList $ event' : eis

onClick :: IORef State -> MouseData -> IO ()
onClick stateRef (MouseData mousePos _ _) = do
  (event', _, _, _, _) <- readIORef stateRef
  case event' of
    NoEvent -> playerMove stateRef mousePos
    EventItemList (Fight{} : _) -> fightMove stateRef mousePos
    _ -> event stateRef event'

play :: Maybe String -> IO ()
play (Just worldStr) =
  case parseWorld worldStr of
    Right world -> do
      Just ce <- elemById "canvas"
      imgs <- loadImages world
      let sMap = startMap world
      let sPoint = startPoint sMap
      let sPicture = drawMap imgs sMap
      let pImg = Player 1 `lookup` imgs

      stateRef <- newIORef (NoEvent,
                            (sMap, sPoint, renderState pImg sPicture),
                            world, imgs, Checkpoint 0)
      _ <- onEvent ce Click $ onClick stateRef
      renderState pImg sPicture sPoint
      event stateRef (eventPoint world sMap sPoint)
    Left s -> changeOutputHTML ("Errors: </br>" ++ s)
play Nothing = changeOutputHTML "World file not loaded"

changeOutputHTML :: String -> IO ()
changeOutputHTML s = do
  Just e <- elemById "output"
  setProp e "innerHTML" s
