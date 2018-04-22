module Main where

import Game
import Haste.Ajax

main :: IO ()
main = do
  -- Loading the map
  ajaxRequest GET "world/world.json" noParams play
