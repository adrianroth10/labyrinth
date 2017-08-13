module Graphics (module World,
                 Haste.Graphics.Canvas.Point,
                 Haste.Graphics.Canvas.Picture,
                 Haste.Graphics.Canvas.Bitmap,
                 width,
                 height,
                 drawMap,
                 renderState,
                 loadImages) where
import World

import Haste.Graphics.Canvas

--constants
width, height, blocks :: Double
width = 512
height = 512
blocks = 6

--colors
black, white, red, yellow, green, blue :: Picture () -> Picture ()
black = color $ RGB 0 0 0
white = color $ RGB 255 255 255
red = color $ RGB 255 0 0
yellow = color $ RGB 255 255 0
green = color $ RGB 0 255 0
blue = color $ RGBA 0 0 255 0.5

drawShape :: (Picture () -> Picture ()) -> Shape () -> Picture ()
drawShape col = col . fill

shapeCircle, shapeRect:: Rect -> Shape ()
shapeCircle (Rect x y w h) = circle (x + w / 2, y + h / 2)
                                    (min (w / 2) (h / 2))
shapeRect (Rect x y w h) = rect (x, y) (x + w, y + h)

-- Either all tiles have bitmaps or no tiles, CHANGE!!
drawTile :: [(Tile, Bitmap)] -> Tile -> Rect -> Picture ()
drawTile [] Start = drawShape yellow . shapeCircle
drawTile [] End = drawShape green . shapeCircle
drawTile [] (Free _) = drawShape black . shapeCircle
drawTile [] (Wall _) = drawShape white . shapeRect
drawTile [] (Event _) = drawShape red . shapeCircle
drawTile imgs tile = drawScaled img
  where Just img = lookup tile imgs

drawTiles :: [(Tile, Bitmap)] -> [Tile] -> [Rect] -> Picture ()
drawTiles _ [] _ = return ()
drawTiles _ _ [] = return ()
drawTiles imgs (t:ts) (r:rs) = do
  drawTile imgs t r
  drawTiles imgs ts rs

mesh :: Double -> [Rect]
mesh c = map (\(x, y) -> Rect x y w h) points
  where
    w = width / blocks
    h = height / blocks
    pointMul (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
    coordinates = [(x, y) | y <- [0..c - 1], x <- [0..c - 1]]
    points = map (pointMul (w, h)) coordinates

drawMap :: [(Tile, Bitmap)] -> MapContent' -> Picture ()
drawMap imgs (c, tiles) = drawTiles imgs tiles (mesh c)

drawPlayer :: Maybe Bitmap -> Picture ()
drawPlayer Nothing = drawShape blue $ circle (width / 2, height / 2)
                                             (width / blocks / 2 - 10)
drawPlayer (Just img) = drawScaled img playerRect
  where
    w = width / blocks
    h = height / blocks
    playerRect = Rect (width / 2 - w / 2) (height / 2 - h / 2) w h

translateMap :: Double -> Double -> Picture () -> Picture ()
translateMap x y picture = translate (x_i, y_i) picture
  where
    x_i = -(x - (blocks - 1) / 2) * width / blocks
    y_i = -(y - (blocks - 1) / 2) * height / blocks


---------------------------------Impure--------------------------------
loadImages :: World -> IO [(Tile, Bitmap)]
loadImages [] = return []
loadImages ((_, TileItem "" _):xti) = loadImages xti
loadImages ((t, TileItem s _):xti) = (:) <$> fmap (\img -> (t, img))
                                             (loadBitmap s) <*>
                                             loadImages xti
loadImages ((_, MapContent _):xti) = loadImages xti

renderState :: Picture () -> Point -> IO ()
renderState mapPicture (x, y) = do 
  Just c <- getCanvasById "canvas"
  render c $ do
    translateMap x y mapPicture
    drawPlayer Nothing
