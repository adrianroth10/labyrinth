module Graphics (mkCanvas, renderState, appendChild, documentBody, fromElem) where

import Map

import Haste
import Haste.DOM
import Haste.Graphics.Canvas

--Constants
width, height :: Double
radius :: Double -> Double
size :: Double -> Point
width = 512
height = 512
radius = (/) (width / 2)
size blocks = (width / blocks, height / blocks)

displacements :: Double -> [Point]
displacements blocks = [(x, y) | y <- [0..(blocks - 1)], x <- [0..(blocks - 1)]]

points :: Double -> [Point]
points blocks = map (pointMul (size blocks)) (displacements blocks)

black, red, yellow, green, blue :: Picture () -> Picture () 
black = color (RGB 0 0 0)
red = color (RGB 255 0 0)
yellow = color (RGB 255 255 0)
green = color (RGB 0 255 0)
blue = color (RGB 0 0 255)

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

--unMatrixify :: [[a]] -> [a]
--unMatrixify = foldl1 (++)

gamePicture :: Double -> Map -> Picture ()
gamePicture blocks m = drawTiles (foldl1 (++) m) (points blocks) blocks

drawTiles :: [Tile] -> [Point] -> Double -> Picture ()
drawTiles [] _ _ = return ()
drawTiles (t:ts) (p:ps) blocks = drawTile Start p 4
  --drawTile t p blocks
  --drawTiles ts ps blocks

drawTile :: Tile -> Point -> Double -> Picture ()
drawTile Start = drawCircle yellow
drawTile End = drawCircle green
drawTile Wall = drawRect black
drawTile (Event _) = drawCircle red

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
