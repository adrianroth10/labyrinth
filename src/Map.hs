module Map where

import Parser

data Map = Map [[Tile]] deriving Show
data Tile = Free | Start | End | Wall | Event Int deriving Show

createMap :: String -> Map
createMap = matrixify . (number # number # content)

matrixify' :: Int -> [Tile] -> [[Tile]]
matrixify' _ [] = []
matrixify' c xs = (take c xs) : matrixify' c (drop c xs)

matrixify :: Maybe (((Integer, Integer), [Tile]), String) -> Map
matrixify p = case p of
               Just(((r, c), xs), []) -> Map $ matrixify' (fromInteger c) xs
               Just(s, cs) -> error ("garbage '"++cs++"'")
               Nothing -> error "Nothing"

content :: Parser [Tile]
content = (number # content >-> prepend) ! Parser.return []

prepend :: (Integer, [Tile]) -> [Tile]
prepend (x, xs) = tile x:xs

tile :: Integer -> Tile
tile 0 = Free
tile 1 = Start
tile 2 = End
tile 3 = Wall
tile n = Event $ fromInteger $ n - 3

a = "3\n3\n0 1 2\n3 4 5\n0 0 0"
