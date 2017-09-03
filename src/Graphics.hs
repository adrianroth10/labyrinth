module Graphics (Haste.Graphics.Canvas.Point,
                 Haste.Graphics.Canvas.Picture,
                 Haste.Graphics.Canvas.Bitmap,
                 Haste.Graphics.Canvas.translate,
                 -- General
                 (|+|),
                 (|*|),
                 Imgs,
                 width,
                 height,
                 block,
                 fullBlack,
                 fullWhite,
                 loadImages,
                 renderState,
                 renderStateOnTop,
                 -- Map
                 drawMap,
                 -- Text
                 parseDrawText,
                 drawText,
                 drawFullText,
                 -- Fight
                 drawPlayers,
                 mergePlayerPictures,
                 updatePlayers) where
import World

import Haste.Graphics.Canvas

import Data.Maybe

type Imgs = [(Tile, Bitmap)]

--constants
width, height, block :: Double
width = 800
height = 500
block = 50

--colors
black, white, red, yellow, blue :: Picture () -> Picture ()
black = color $ RGB 0 0 0
white = color $ RGB 255 255 255
red = color $ RGB 255 0 0
yellow = color $ RGB 255 255 0
blue = color $ RGBA 0 0 255 0.5

(|+|) :: Num a => (a, a) -> (a, a) -> (a, a)
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(|*|) :: Num a => (a, a) -> (a, a) -> (a, a)
(|*|) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

textHelper20px :: Point -> String -> Picture ()
textHelper20px = ((.) (font "20px italic Monospace")) . text 

drawShape :: (Picture () -> Picture ()) -> Shape () -> Picture ()
drawShape col = col . fill

shapeCircle, shapeRect:: Rect -> Shape ()
shapeCircle (Rect x y w h) = circle (x + w / 2, y + h / 2)
                                    (min (w / 2) (h / 2))
shapeRect (Rect x y w h) = rect (x, y) (x + w, y + h)


-------------------------DrawText--------------------------------------
padding :: Double
padding = 10
textRect :: Rect
textRect = Rect (width / 2 - 250) (height - 100 - padding)
                500 100
drawTextBox :: Picture ()
drawTextBox = do
  drawShape white $ rect (rect_x textRect + padding, rect_y textRect)
                         (rect_x textRect + rect_w textRect - padding,
                          rect_y textRect + rect_h textRect)
  drawShape white $ rect (rect_x textRect, rect_y textRect + padding)
                         (rect_x textRect + rect_w textRect,
                          rect_y textRect + rect_h textRect - padding)
  drawShape white $ circle (rect_x textRect + padding,
                            rect_y textRect + padding) padding
  drawShape white $ circle 
                          (rect_x textRect + rect_w textRect - padding,
                           rect_y textRect + padding) padding
  drawShape white $ circle 
                          (rect_x textRect + rect_w textRect - padding,
                           rect_y textRect + rect_h textRect - padding)
                          padding
  drawShape white $ circle 
                          (rect_x textRect + padding,
                           rect_y textRect + rect_h textRect - padding)
                          padding
  stroke $ rect (rect_x textRect + padding,
                 rect_y textRect + padding)
                (rect_x textRect + rect_w textRect - padding,
                 rect_y textRect + rect_h textRect - padding)

textPoint1, textPoint2 :: Point
textPoint1 = (rect_x textRect + 2 * padding,
              rect_y textRect + 3.5 * padding)
textPoint2 = (rect_x textRect + 2 * padding,
              rect_y textRect + rect_h textRect / 2 + 2.5 * padding)

parseDrawText' :: String -> [[String]] -> (String, String)
parseDrawText' "" [] = ("", "")
parseDrawText' ps [] = (ps, "")
parseDrawText' ps [[]] = (ps, "")
parseDrawText' ps ([]:xs) = (ps, unlines (map unwords xs))
parseDrawText' [] ((w:ws):xs) = parseDrawText' w (ws:xs)
parseDrawText' ps ((w:ws):xs) 
  | length ps + length w < maxLength = parseDrawText' (ps ++ " " ++ w) (ws:xs)
  | otherwise = (ps, unlines (map unwords ((w:ws):xs)))
    where maxLength = 45

parseDrawText :: String -> (String, String)
parseDrawText = parseDrawText' "" . map words . lines

drawText :: (String, String) -> Picture ()
drawText (s1, s2) = do
  drawTextBox
  textHelper20px textPoint1 s1
  textHelper20px textPoint2 s2

headingPoint :: Double -> Point
headingPoint l = (width / 2 - l * 13, 5 * padding)
fullTextPoint :: Double -> Point
fullTextPoint i = (fst textPoint1, padding * (5 + 5 * i))

drawFullText :: String -> [String] -> (Point, Point, Picture ())
drawFullText h s = (p1, p2, do
  drawShape (color (RGB 0 0 0))
            (shapeRect (Rect 0 0 width (max height (-(snd p2)))))

  white $ font "40px italic Monospace" $
          text (headingPoint (fromIntegral (length h))) h
  white $ mapM_ (uncurry textHelper20px)
                (zip [fullTextPoint i | i <- [1..lS]] s))
    where
      p1 = (0, 0)
      p2 = (0, -snd (fullTextPoint (lS + 1)))
      lS = fromIntegral $ length s
-----------------------------------------------------------------------

---------------------------Fading--------------------------------------
fullRect :: Rect
fullRect = Rect 0 0 width height

fullBlack :: Double -> Picture ()
fullBlack a = drawShape (color (RGBA 0 0 0 a))
                        (shapeRect fullRect)
fullWhite :: Double -> Picture ()
fullWhite a = drawShape (color (RGBA 255 255 255 a))
                        (shapeRect fullRect)
-----------------------------------------------------------------------

---------------------------DrawMap-------------------------------------
-- If no image has been specified
drawTile' :: Tile -> Rect -> Picture ()
drawTile' Start = drawShape yellow . shapeCircle
drawTile' (Free _) = drawShape black . shapeCircle
drawTile' (Wall _) = drawShape white . shapeRect
drawTile' (Event _) = drawShape red . shapeCircle
drawTile' _ = error "Cannot be drawn from tile info"

drawTile :: Imgs -> Tile -> Rect -> Picture ()
drawTile imgs tile 
  | isJust img = drawScaled (fromJust img)
  | otherwise  = drawTile' tile
  where img = lookup tile imgs

drawTiles :: Imgs -> [(Tile, Rect)] -> Picture ()
drawTiles imgs tileAndPoints = mapM_ (uncurry (drawTile imgs))
                                     tileAndPoints

mesh :: Double -> [Rect]
mesh c = map (\(x, y) -> Rect x y block block) points
  where
    pointMul (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
    coordinates = [(x, y) | y <- [0..], x <- [0..c - 1]]
    points = map (pointMul (block, block)) coordinates

drawMap :: Imgs -> MapContent' -> Picture ()
drawMap imgs (c, tiles) = drawTiles imgs $ zip tiles (mesh c)
-----------------------------------------------------------------------

drawPlayer :: Maybe Bitmap -> Picture ()
drawPlayer Nothing = drawShape blue $ circle (width / 2, height / 2)
                                   (block / 2 - 10)
drawPlayer (Just img) = drawScaled img playerRect
  where
    playerRect = Rect (width / 2 - block / 2) (height / 2 - block / 2)
                      block block

translateMap :: Double -> Double -> Picture () -> Picture ()
translateMap x y picture = translate (x_i, y_i) picture
  where
    x_i = -(x - (width / block - 1) / 2) * block
    y_i = -(y - (height / block - 1) / 2) * block

---------------------------Fight---------------------------------------
wh, x1', x2' :: Double
wh = min width height / 3
x1' = width / 6
x2' = width * 2 / 3

localNamePoint, localHpPoint, localStat1Point, localStat2Point :: Point
globalPlayer1Point, globalPlayer2Point :: Point
localNamePoint = (0, height / 10)
localHpPoint = (0, height / 10 + 1)
localStat1Point = (x2', 0)
localStat2Point = (x1', 0)
globalPlayer1Point = (0, height / 3) |+| globalPlayer2Point
globalPlayer2Point = (0, height - 430)

intToDouble :: Int -> Double
intToDouble = fromIntegral
basePoint, charSize :: Point 
basePoint = (50, 50)
charSize = (4, 4)
globalMovesPoint1, globalMovesPoint2 :: Int -> Point
globalMovesPoint3, globalMovesPoint4 :: Int -> Point
globalMovesPoint1 l' = basePoint |+| ((-l, l) |*| charSize)
  where l = intToDouble l'
globalMovesPoint2 l' = (width, 0) |+| ((-1, 1) |*| basePoint) |+|
                       ((-l, -l) |*| charSize)
  where l = intToDouble l'
globalMovesPoint3 l' = (0, height) |+| ((1, -1) |*| basePoint) |+|
                       ((-l, -l) |*| charSize)
  where l = intToDouble l'
globalMovesPoint4 l' = (width, height) |+|
                      ((-1, -1) |*| basePoint) |+|
                      ((-l, l) |*| charSize)
  where l = intToDouble l'

localPlayer1Rect, localPlayer2Rect :: Rect
localPlayer1Rect = Rect x1' 0 wh wh
localPlayer2Rect = Rect x2' 0 wh wh

doubleToInteger :: Double -> Integer
doubleToInteger = floor
drawHp :: Double -> Picture ()
drawHp hp = do
  textHelper20px localHpPoint $ show $ doubleToInteger hp
  fill $ rect (localHpPoint |+| textHpDiff)
              (localHpPoint |+| textHpDiff |+| (hp * 1.5, 10))
    where textHpDiff = (0, -30)

drawPlayerName :: TileItem -> Picture ()
drawPlayerName (PlayerItem _ name _) = do
  textHelper20px localNamePoint name
drawPlayerName _ = return ()

drawPlayers :: (Bitmap, Bitmap) -> (TileItem, TileItem) -> 
               ((Point, Picture ()), (Point, Picture ()))
drawPlayers (p1Img, p2Img) (pItem1, pItem2) =
  ((globalPlayer1Point, do
    whiteBack
    drawScaled p1Img localPlayer1Rect
    translate localStat1Point $ drawPlayerName pItem1)
  , (globalPlayer2Point, do
    whiteBack
    translate localStat2Point $ drawPlayerName pItem2
    drawScaled p2Img localPlayer2Rect))
  where whiteBack = drawShape white $ shapeRect $ Rect 0 0 width wh

mergePlayerPictures :: (Picture (), Picture ()) -> Moves-> Picture ()
mergePlayerPictures (pImg1, pImg2) moves = do
  fullWhite 1
  translate globalPlayer1Point pImg1 
  translate globalPlayer2Point pImg2
  drawMoves moves

updatePlayers :: Picture () -> Vector -> Picture ()
updatePlayers base (hp1, hp2) = do
  base
  translate (globalPlayer1Point |+| localStat1Point |+| localHpPoint) $
            drawHp hp1
  translate (globalPlayer2Point |+| localStat2Point |+| localHpPoint) $
            drawHp hp2

drawMoves :: Moves -> Picture ()
drawMoves moves = do
  mapM_ (weakGray . fill . circle (0, 0)) [115..125]
  mapM_ (weakGray . fill . circle (width, 0)) [115..125]
  mapM_ (weakGray . fill . circle (width, height)) [115..125]
  mapM_ (weakGray . fill . circle (0, height)) [115..125]
  translate (globalMovesPoint1 (length move1)) $
                        rotate (-pi / 4) $ textHelper20px (0, 0) move1
  translate (globalMovesPoint2 (length move2)) $
                        rotate (pi / 4) $ textHelper20px (0, 0) move2
  translate (globalMovesPoint3 (length move3)) $
                        rotate (pi / 4) $ textHelper20px (0, 0) move3
  translate (globalMovesPoint4 (length move4)) $
                        rotate (-pi / 4) $ textHelper20px (0, 0) move4
  where
    weakGray = color $ RGBA 0 0 0 0.05
    mNames = map (\(name, _, _) -> name) moves  
    head' [] = ""
    head' s = head s
    move1 = head' mNames 
    move2 = head' $ drop 1 mNames
    move3 = head' $ drop 2 mNames
    move4 = head' $ drop 3 mNames
-----------------------------------------------------------------------

---------------------------------Impure--------------------------------
loadImages :: World -> IO Imgs
loadImages [] = return []
loadImages ((_, TileItem "" _):xti) = loadImages xti
loadImages ((t, TileItem s _):xti) = (:) <$> fmap (\img -> (t, img))
                                             (loadBitmap s) <*>
                                             loadImages xti
loadImages ((t, PlayerItem s _ _):xti) = (:) <$> fmap (\img -> (t, img))
                                             (loadBitmap s) <*>
                                             loadImages xti
loadImages ((_, _):xti) = loadImages xti

renderStateOnTop :: Picture () -> Point -> IO ()
renderStateOnTop picture point = do 
  Just c <- getCanvasById "canvas"
  renderOnTop c $ translate point picture 

renderState :: Maybe Bitmap -> Picture () -> Point -> IO ()
renderState pImg mapPicture (x, y) = do 
  Just c <- getCanvasById "canvas"
  render c $ do
    translateMap x y mapPicture
    drawPlayer pImg
