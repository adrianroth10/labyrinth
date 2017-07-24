module Map (Map,
            Tile (Free, Start, End, Wall, Event),
            parseMap) where

import Parser

--change to type?
type Map = [[Tile]]
data Tile = Free | Start | End | Wall | Event Int deriving (Eq, Show)

------------Parsing---------------------
parseMap :: String -> Maybe Map
parseMap = matrixify . (number # number # content)

matrixify' :: Int -> [a] -> [[a]]
matrixify' _ [] = []
matrixify' c xs = (take c xs) : matrixify' c (drop c xs)

--add cases to check for only one start and end
matrixify :: Maybe (((Integer, Integer), [Tile]), String) -> Maybe Map
matrixify Nothing = Nothing
matrixify (Just (((r, c), xs), s))
  | fromInteger (r * c) == length xs =
                            Just $ matrixify' (fromInteger c) xs
  | otherwise = Nothing

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
----------------------------------------