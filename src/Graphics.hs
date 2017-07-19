module Graphics (mkCanvas, renderState, appendChild, documentBody, fromElem) where

import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Data.IORef

--Constants
width, height, blocks :: Double
size :: Point
blocks = 4
width = 500
height = 500
size = (width / blocks, height / blocks)

white :: Picture () -> Picture () 
white = color (RGB 255 255 255)
gray :: Picture () -> Picture () 
gray = color (RGB 127 127 127)
blue :: Picture () -> Picture () 
blue = color (RGB 0 0 255)
red :: Picture () -> Picture () 
red = color (RGB 255 0 0)

mkCanvas :: IO Elem
mkCanvas = do
  canvas <- newElem "canvas"
  setProp canvas "width" (show width)
  setProp canvas "height" (show height)
  setStyle canvas "display" "block"
  setStyle canvas "border" "1px solid black"
  setStyle canvas "margin" "0px auto 0 auto"
  setStyle canvas "backgroundColor" "black"
  return canvas

renderState :: Canvas -> IO ()
renderState canvas = render canvas gamePicture 

gamePicture :: Picture ()
gamePicture = drawStart (0, 0)

drawStart  :: Point -> Picture ()
drawStart pt = red $ do
  fill $ rect pt (pointMove pt size)

--drawEnd  :: Point -> Picture ()
--drawWall  :: Point -> Picture ()
--drawEvent  :: Point -> Picture ()

--rewrite pointfree?
pointMove :: Point -> Point -> Point
pointMove (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
