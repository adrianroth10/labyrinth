module Main where

import Graphics
import Map
import Game

import Haste.Events
import Haste.DOM
import Data.IORef

m :: Map
m = (4, [Wall, Free, Wall, Event event1,
         Wall, Free, Free, Start,
         Wall, Free, Wall, Free,
         Wall, Event event2, Wall, Free])

event1, event2 :: String
event1 = "This is an event on two lines\nThis is the second line, WAOW!"
event2 = "Heres a link to click www.google.se" -- if this works I will be amazed


main :: IO ()
main = do
  Just ce <- elemById "canvas"
  Just c <- fromElem ce

  stateRef <- newIORef (startP, Nothing)
  handle <- onEvent
          ce
          Click $
          movePlayer (renderState c picture) 
                     (((.) (validState m)) . updateState width height)
                     (eventState ce m (renderState c picture))
                     stateRef

  renderState c picture startP
  writeIORef stateRef (startP, Just handle)
  where
    startP = startPoint m
    picture = drawMap m
