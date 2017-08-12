module Map (Map,
            MapInput,
            Tile (Free, Start, End, Wall, Event),
            eqTile,
            parseMap) where

import Parser

type Map = (Double, [Tile])
type MapInput = (Tile, (String, String))
data Tile = Start | End | Free Integer |
            Wall Integer | Event Integer deriving (Eq, Show)

eqTile :: Tile -> Tile -> Bool
eqTile Start Start = True
eqTile End End = True
eqTile (Free _) (Free _) = True
eqTile (Wall _) (Wall _) = True
eqTile (Event _) (Event _) = True
eqTile _ _ = False

tile :: Integer -> Tile
tile n
  | n == 0 = Start
  | n == 1 = End
  | n >= 10 && n < 20 = Free (n - 9)
  | n >= 20 && n < 30 = Wall (n - 19)
  | n >= 30 = Event (n - 29)
  | otherwise = error "Not a valid Tile"

prepend :: (a, [a]) -> [a]
prepend (x, xs) = x:xs

inputContent :: Parser [String]
inputContent = accept "END" -# Parser.return [] !
               line # inputContent >-> prepend

parseInput :: Parser MapInput
parseInput = accept "Start" -# accept "IMG" -#
             (var # (inputContent >-> unlines)) >->
             (\(img, input) -> (Start, (img, input))) !
             accept "Start" -# (inputContent >-> unlines) >->
             (\input -> (Start, ("", input))) !
             accept "End" -# accept "IMG" -#
             (var # (inputContent >-> unlines)) >->
             (\(img, input) -> (End, (img, input))) !
             accept "End" -# (inputContent >-> unlines) >->
             (\input -> (End , ("", input))) !
             accept "Free" -# (number #- accept "IMG" # var) >->
             (\(n, img) -> (Free n, (img, ""))) !
             accept "Free" -# number >-> (\n -> (Free n, ("", ""))) !
             accept "Wall" -# (number #- accept "IMG" # var) >->
             (\(n, img) -> (Wall n, (img, ""))) !
             accept "Wall" -# number >-> (\n -> (Wall n, ("", ""))) !
             accept "Event" -#
             (number #- accept "IMG" # var #
              (inputContent >-> unlines)) >->
             (\((n, img), input) -> (Event n, (img, input))) !
             accept "Event" -#
             (number # (inputContent >-> unlines)) >->
             (\(n, input) -> (Event n, ("", input)))

parseInputs :: Parser [MapInput]
parseInputs = parseInput # parseInputs >-> prepend !  Parser.return []

format :: Maybe (((Integer, Integer), [Tile]), String) ->
          Maybe (Map, [MapInput])
format Nothing = Nothing
format (Just (((r, c), tiles), s))
  | fromInteger (r * c) /= length tiles = Nothing
  | length starts /= 1 = Nothing
  | length ends /= 1 = Nothing
  | otherwise = fmap (\inputs' -> ((fromInteger c, tiles), inputs')) inputs
  where
    starts = filter ((==) Start) tiles
    ends = filter ((==) End) tiles
    inputs = case parseInputs s of 
               Nothing -> Nothing
               Just (inputs', "") -> Just inputs'
               Just (_, _) -> Nothing -- Garbarge not parsed error

mapContent :: Parser [Integer]
mapContent = (number # mapContent >-> prepend) ! Parser.return []

parseMap :: String -> Maybe (Map, [MapInput])
parseMap = format . (number # number # (mapContent >-> (map tile)))
