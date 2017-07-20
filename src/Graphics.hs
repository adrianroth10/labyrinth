module Graphics (mkCanvas, renderState, appendChild, documentBody, fromElem) where

import Map

import Haste
import Haste.DOM
import Haste.Graphics.Canvas

--Constants
width, height :: Double
radius :: Double -> Double
size :: Double -> Point
width = 500
height = 500
radius = (/) (width / 2)
size blocks = (width / blocks, height / blocks)

--x :: Double -> [Point]

black, gray, blue, red :: Picture () -> Picture () 
black = color (RGB 0 0 0)
gray = color (RGB 127 127 127)
blue = color (RGB 0 0 255)
red = color (RGB 255 0 0)

mkCanvas :: IO Elem
mkCanvas = do
  canvas <- newElem "canvas"
  setProp canvas "width" (show width)
  setProp canvas "height" (show height)
  setStyle canvas "display" "block"
  setStyle canvas "border" "1px solid black"
  setStyle canvas "margin" "0px auto 0 auto"
  setStyle canvas "backgroundColor" "white"
  return canvas

renderState :: Canvas -> Double -> Map -> IO ()
renderState canvas = ((.) (render canvas)) . gamePicture

gamePicture :: Double -> Map -> Picture ()
gamePicture blocks map = do 
  drawTile Start (0, 0) blocks
  drawTile End (10, 10) blocks
  drawTile Wall (20, 20) blocks
  drawTile (Event 1) (100, 100) blocks

drawTile :: Tile -> Point -> Double -> Picture ()
drawTile Start = drawRect red
drawTile End = drawRect blue
drawTile Wall = drawRect black
drawTile (Event _) = drawCircle gray

drawRect, drawCircle :: (Picture () -> Picture ()) ->
                        Point ->
                        Double ->
                        Picture ()
drawRect c pt blocks = c $ fill $ rect pt (pointAdd pt (size blocks))
drawCircle c pt blocks = c $ fill $ circle (pointAdd pt (pointMul (0.5, 0.5) (size blocks))) (radius blocks)

--Pointstuff
map2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
map2 f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)
pointAdd, pointMul :: Point -> Point -> Point
pointAdd = map2 (+)
pointMul = map2 (*)
