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
      stateRef <- newIORef $ startPoint m
      onEvent ce
              Click $
              movePlayer (renderState c (drawMap m)) 
                         (((.) (validState m)) . updateState width height)
                         (eventState m outputText)
                         stateRef
      renderState c (drawMap m) (startPoint m)
    Nothing -> error "Map parsing wrong"
whenLoaded Nothing = error "Map not loaded"

main :: IO ()
main = do
  ajaxRequest GET "map/l1.txt" noParams whenLoaded
