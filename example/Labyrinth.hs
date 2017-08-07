module Main where

import Graphics
import Map
import Game

import Haste.Ajax
import Haste.DOM
import Haste.Events

import Data.IORef

whenLoaded :: Maybe String -> IO ()
whenLoaded (Just mapStr) = do 
  case parseMap mapStr of
    Just m -> do
      Just ce <- elemById "canvas"
      Just c <- fromElem ce
      Just divElem <- elemById "output"
      stateRef <- newIORef $ (startPoint m, False)
      onEvent ce
              Click $
              movePlayer (renderState c (drawMap m)) 
                         (((.) (validPoint m)) . updatePoint width height)
                         (eventPoint m (changeInnerHTML divElem))
                         stateRef
      renderState c (drawMap m) (startPoint m)
      eventPoint m (changeInnerHTML divElem) (startPoint m)
    Nothing -> error "Map parsing error"
whenLoaded Nothing = error "Map not loaded"

main :: IO ()
main = do
  -- Loading the map
  ajaxRequest GET "map/map.txt" noParams whenLoaded
