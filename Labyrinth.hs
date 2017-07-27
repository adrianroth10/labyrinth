module Main where

import Graphics
import Map
import Game

import Haste.Events
import Haste.DOM
import Data.IORef

m :: Map
m = (4, [Wall, Free, Wall, Event,
         Wall, Free, Start, Free,
         Wall, Wall, Wall, Free,
         Wall, Event, Wall, Free])


main :: IO ()
main = do
  Just ce <- elemById "canvas"
  Just c <- fromElem ce

  stateRef <- newIORef state
  onEvent ce
          Click $
          movePlayer (renderState c picture) 
                     (((.) (validState m)) . updateState width height)
                     stateRef
  renderState c picture state
  where
    state = startState m
    picture = drawMap m
