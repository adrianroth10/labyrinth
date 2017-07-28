module Graphics (width,
                 height,
                 drawMap,
                 renderState) where
import Map

import Haste.Graphics.Canvas

--constants
width, height, blocks :: Double
width = 512
height = 512
blocks = 4

--colors
black, white, red, yellow, green, blue :: Picture () -> Picture ()
black = color $ RGB 0 0 0
white = color $ RGB 255 255 255
red = color $ RGB 255 0 0
yellow = color $ RGB 255 255 0
green = color $ RGB 0 255 0
blue = color $ RGB 0 0 255

drawShape :: (Picture () -> Picture ()) -> Shape () -> Picture ()
drawShape col = col . fill

shapeCircle, shapeRect:: Rect -> Shape ()
shapeCircle (Rect x y w h) = circle (x + w / 2, y + h / 2)
                                    (min (w / 2) (h / 2))
shapeRect (Rect x y w h) = rect (x, y) (x + w, y + h)

drawTile :: Tile -> Rect -> Picture ()
drawTile Free = drawShape white . shapeRect 
drawTile Start = drawShape yellow . shapeCircle
drawTile End = drawShape green . shapeCircle
drawTile Wall = drawShape black . shapeRect
drawTile (Event _) = drawShape red . shapeCircle

drawTiles :: [Tile] -> [Rect] -> Picture ()
drawTiles [] _ = return ()
drawTiles _ [] = return ()
drawTiles  (t:ts) (r:rs) = do
  drawTile t r
  drawTiles ts rs

mesh :: Double -> [Rect]
mesh c = map (\(x, y) -> Rect x y w h) points
  where
    w = width / blocks
    h = height / blocks
    pointMul (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
    coordinates = [(x, y) | y <- [0..c - 1], x <- [0..c - 1]]
    points = map (pointMul (w, h)) coordinates

drawMap :: Map -> Picture ()
drawMap (c, tiles) = drawTiles tiles (mesh c)

drawPlayer :: Picture ()
drawPlayer = drawShape blue $ circle (width / 2, height / 2)
                                     (width / blocks / 2 - 10)

translateMap :: Double -> Double -> Picture () -> Picture ()
translateMap x y picture = translate (x_i, y_i) picture
  where
    x_i = -(x - (blocks - 1) / 2) * width / blocks
    y_i = -(y - (blocks - 1) / 2) * height / blocks

renderState :: Canvas -> Picture () -> Point -> IO ()
renderState c picture (x, y) = render c $ do
    translateMap x y picture
    drawPlayer
