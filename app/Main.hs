module Main where

import Map
import System.IO

mapFile = "maps/l1.txt"

main :: IO ()
main = do
          handle <- openFile mapFile ReadMode
          contents <- hGetContents handle
          let map = maybe (error "map parsing error")
                          id
                          (parseMap contents)
          putStrLn $ show map
          hClose handle
