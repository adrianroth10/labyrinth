module Main where

import Graphics
import Map
import Game

import Haste.Events
import Haste.DOM
import Haste.Graphics.Canvas
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

  let state = startState m
  let picture = drawMap m
  stateRef <- newIORef state
  onEvent ce Click $ movePlayer c picture stateRef
  render c $ do 
    translateMap (x_coord state) (y_coord state) picture
    drawPlayer
