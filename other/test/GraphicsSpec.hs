module GraphicsSpec where

import Haste
import Haste.DOM
import Haste.Graphics.Canvas

width, height :: Double
width = 500
height = 500

main :: IO ()
main = do
  canvasElem <- mkCanvas
  appendChild documentBody canvasElem
  Just canvas <- fromElem canvasElem
  renderState canvas

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
  setStyle canvas "backgroundColor" "black"
  return canvas

renderState :: Canvas -> IO ()
renderState canvas = render canvas gamePicture

gamePicture :: Picture ()
gamePicture = do
  red $ fill $ circle (0, 0) 100
