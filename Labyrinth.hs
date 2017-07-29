{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics
import Map
import Game

import Haste.DOM
import Haste.Events
import Haste.Foreign

import Data.IORef


m :: Map
m = (4, [Wall, Free, Wall, Event event1,
         Wall, Free, Free, Start,
         Wall, Free, Wall, Free,
         Wall, Event event2, Wall, Free])

event1, event2 :: String
event1 = "<p>This is an event on two lines</p><p>This is the second line, WAOW!</p>"
event2 = "<p>Heres a link to <a target=\"_blank\" href=https://www.rothfastigheter.se>click</a></p>"

outputText :: String -> IO ()
outputText = ffi "(function(str){document.getElementById(\"output\").innerHTML = str;})"

main :: IO ()
main = do
  Just ce <- elemById "canvas"
  Just c <- fromElem ce

  stateRef <- newIORef startP
  onEvent ce
          Click $
          movePlayer (renderState c picture) 
                     (((.) (validState m)) . updateState width height)
                     (eventState m outputText)
                     stateRef

  renderState c picture startP
  where
    startP = startPoint m
    picture = drawMap m
