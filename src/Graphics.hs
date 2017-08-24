module Graphics (Haste.Graphics.Canvas.Point,
                 Haste.Graphics.Canvas.Picture,
                 Haste.Graphics.Canvas.Bitmap,
                 Imgs,
                 width,
                 height,
                 block,
                 fullBlack,
                 fullWhite,
                 drawMap,
                 parseDrawText,
                 drawText,
                 drawFullText,
                 renderState,
                 renderStateOnTop,
                 loadImages) where
import World

import Haste.Graphics.Canvas

import Data.Maybe

type Imgs = [(Tile, Bitmap)]

--constants
width, height, block :: Double
width = 712
height = 512
block = 50

--colors
black, white, red, yellow, blue :: Picture () -> Picture ()
black = color $ RGB 0 0 0
white = color $ RGB 255 255 255
red = color $ RGB 255 0 0
yellow = color $ RGB 255 255 0
blue = color $ RGBA 0 0 255 0.5

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
  font "20px italic Monospace" $ text textPoint1 s1
  font "20px italic Monospace" $ text textPoint2 s2

headingPoint :: Double -> Point
headingPoint l = (width / 2 - l * 13, 5 * padding)
fullTextPoint :: Double -> Point
fullTextPoint i = (fst textPoint1, padding * (5 + 5 * i))

drawFullText :: String -> [String] -> (Point, Point, Picture ())
drawFullText h s = (p1, p2, do
  drawShape (color (RGB 0 0 0))
            (shapeRect (Rect 0 0 width (snd p2 * block)))

  white $ font "40px italic Monospace" $
          text (headingPoint (fromIntegral (length h))) h
  mapM_ (uncurry ((.) (white . font "20px italic Monospace") . text))
        (zip [fullTextPoint i | i <- [1..lS]] s))
    where
      p1 = (0, 0)
      p2 = (0, snd (fullTextPoint (lS + 1)) / block)
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
    playerRect = Rect (width / 2 - block / 2) (height / 2 - block / 2) block block

translateMap :: Double -> Double -> Picture () -> Picture ()
translateMap x y picture = translate (x_i, y_i) picture
  where
    x_i = -(x - (width / block - 1) / 2) * block
    y_i = -(y - (height / block - 1) / 2) * block

---------------------------------Impure--------------------------------
loadImages :: World -> IO Imgs
loadImages [] = return []
loadImages ((_, TileItem "" _):xti) = loadImages xti
loadImages ((t, TileItem s _):xti) = (:) <$> fmap (\img -> (t, img))
                                             (loadBitmap s) <*>
                                             loadImages xti
loadImages ((t, PlayerItem s):xti) = (:) <$> fmap (\img -> (t, img))
                                             (loadBitmap s) <*>
                                             loadImages xti
loadImages ((_, _):xti) = loadImages xti

renderStateOnTop :: Picture () -> Point -> IO ()
renderStateOnTop picture (x, y) = do 
  Just c <- getCanvasById "canvas"
  renderOnTop c $ translateMap (x + (width / block -1) / 2)
                               (y + (height / block - 1) / 2)
                               picture 

renderState :: Maybe Bitmap -> Picture () -> Point -> IO ()
renderState pImg mapPicture (x, y) = do 
  Just c <- getCanvasById "canvas"
  render c $ do
    translateMap x y mapPicture
    drawPlayer pImg
