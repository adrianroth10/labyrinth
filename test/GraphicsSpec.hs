module GraphicsSpec where

import Map
import Graphics

theMap = Map [[Free, Start, End],
       [Wall, Wall, Wall],
       [Event 1, Event 2, Event 3]]

main :: IO ()
main = do
          canvasElem <- mkCanvas
          appendChild documentBody canvasElem
          Just canvas <- fromElem canvasElem
          renderState canvas 3 theMap
