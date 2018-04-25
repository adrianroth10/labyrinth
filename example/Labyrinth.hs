module Main where

import Game
import Haste.Ajax

main :: IO ()
main =
  -- Loading the map
  ajaxRequest GET "world/world.json" noParams play
