--import Haste
import Haste.Graphics.Canvas

type Map = (Double, [Tile])
data Tile = Free | Start | End | Wall | Event deriving (Eq, Show)
data State = State {
    x_coord :: Double,
    y_coord :: Double
  }

--constants
width, height :: Double
width = 512
height = 512
initState :: State
initState = State {
    x_coord = width / 2,
    y_coord = height / 2
  }

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
drawTile Event = drawShape red . shapeCircle

drawTiles :: [Tile] -> [Rect] -> Picture ()
drawTiles [] _ = return ()
drawTiles _ [] = return ()
drawTiles  (t:ts) (r:rs) = do
  drawTile t r
  drawTiles ts rs

mesh :: Double -> [Rect]
mesh c = map (\(x, y) -> Rect x y w h) points
  where
    w = width / c
    h = height / c
    pointMul (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
    coordinates = [(x, y) | y <- [0..c - 1], x <- [0..c - 1]]
    points = map (pointMul (w, h)) coordinates

drawMap :: Map -> Picture ()
drawMap (c, tiles) = drawTiles tiles (mesh c)

drawPlayer :: State -> Picture ()
drawPlayer state = drawShape blue $ circle (x_coord state, y_coord state) 50

gamePicture :: Map -> Picture ()
gamePicture m = do 
  drawMap m
  drawPlayer initState

main :: IO ()
main = do
  Just can <- getCanvasById "canvas"
  render can $ gamePicture (3, [Wall, Free, Wall,
                                Wall, Start, Wall,
                                Wall, Wall, Wall])
