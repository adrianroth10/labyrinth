module Main where

import Graphics
import Map
import Game

import Haste
import Haste.Ajax
import Haste.DOM
import Haste.Events

import Data.IORef

whenLoaded :: Maybe String -> IO ()
whenLoaded (Just mapStr) = do 
  case parseMap mapStr of
    Just (m, mInput) -> do
      Just ce <- elemById "canvas"
      Just c <- fromElem ce
      Just outputElem <- elemById "output"
      imgs <- loadImages mInput

      stateRef <- newIORef $ (startPoint m, False)
      onEvent ce
              Click $
              movePlayer (renderState c (drawMap imgs m)) 
                         (((.) (validPoint m)) . updatePoint width height)
                         (eventPoint m mInput (changeInnerHTML outputElem))
                         stateRef
      renderState c (drawMap imgs m) (startPoint m)
      eventPoint m mInput (changeInnerHTML outputElem) (startPoint m)
    Nothing -> alert "Map parsing error"
whenLoaded Nothing = alert "Map file not loaded"

main :: IO ()
main = do
  -- Loading the map
  ajaxRequest GET "map/map.txt" noParams whenLoaded
