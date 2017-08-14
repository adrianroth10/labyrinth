module World (World,
            Tile (Free, Start, End, Wall, Event, Map),
            TileItem (MapContent, TileItem),
            MapContent',
            EventItem (NoEvent, Text, HTMLText, Teleport, EventItemList),
            eqTile,
            parseWorld) where

import Parser

type World = [(Tile, TileItem)]
data Tile = Start | End | Free Integer |
            Wall Integer | Event Integer |
            Map Integer deriving (Eq, Show)
data TileItem = MapContent (Double, [Tile]) |
                TileItem String EventItem deriving (Eq, Show)
type MapContent' = (Double, [Tile])
data EventItem = NoEvent |
                 Text String |
                 HTMLText String |
                 Teleport Tile (Double, Double) |
                 EventItemList [EventItem] deriving (Eq, Show)

end :: String
end = "END"
img :: String
img = "IMG"

eqTile :: Tile -> Tile -> Bool
eqTile Start Start = True
eqTile End End = True
eqTile (Free _) (Free _) = True
eqTile (Wall _) (Wall _) = True
eqTile (Event _) (Event _) = True
eqTile (Map _) (Map _) = True
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

---------------------------------Event---------------------------------
inputContent :: Parser [String]
inputContent = accept "END" -# Parser.return [] !
               line # inputContent >-> prepend

parseEvent :: Parser EventItem
parseEvent = accept "Text" -# inputContent >-> (Text . unlines) !

           accept "HTMLText" -# inputContent >-> (HTMLText . unlines) !

             accept "Teleport" -# accept "Map" -# number #-
             accept "Point" # (number >-> fromInteger) # 
             (number >-> fromInteger) >->
             (\((n, x), y) -> Teleport (Map n) (x, y))

formatEvents :: [EventItem] -> EventItem
formatEvents [] = NoEvent
formatEvents [ei] = ei
formatEvents xei = EventItemList xei

parseEvents' :: Parser [EventItem]
parseEvents' = accept end -# Parser.return [] !
               parseEvent # parseEvents' >-> prepend
parseEvents :: Parser EventItem
parseEvents = parseEvents' >-> formatEvents
-----------------------------------------------------------------------

-----------------------------------Map---------------------------------
validMap :: MapContent' -> Bool
validMap (c, tiles)
  | length tiles `mod` floor c /= 0 = False
  | otherwise = True

mapContent :: Parser [Integer]
mapContent = accept end -# Parser.return [] !
             (number # mapContent >-> prepend)

parseMap :: Parser TileItem
parseMap = (((number >-> fromInteger) # (mapContent >-> (map tile))) ?
           validMap) >-> MapContent
-----------------------------------------------------------------------

-----------------------------------World-------------------------------
parseWorldItem :: Parser (Tile, TileItem)
parseWorldItem = accept "Map" -# number # parseMap >->
                 (\(n, m) -> (Map n, m)) !

                 accept "Start" -# accept img -# var # parseEvents >->
                 (\(image, e) -> (Start, TileItem image e)) !
                 accept "Start" -# parseEvents >->
                 (\e -> (Start, TileItem "" e)) !

                 accept "End" -# accept img -# var # parseEvents >->
                 (\(image, e) -> (End, TileItem image e)) !
                 accept "End" -# parseEvents >->
                 (\e -> (End, TileItem "" e)) !

                 accept "Free" -# number #- accept img # var >->
                 (\(n, image) -> (Free n, TileItem image NoEvent)) !
                 accept "Free" -# number >->
                 (\n -> (Free n, TileItem "" NoEvent)) !

                 accept "Wall" -# number #- accept img # var >->
                 (\(n, image) -> (Wall n, TileItem image NoEvent)) !
                 accept "Wall" -# number >->
                 (\n -> (Wall n, TileItem "" NoEvent)) !

                 accept "Event" -#
                 number #- accept img # var # parseEvents >->
                 (\((n, image), e) -> (Event n, TileItem image e)) !
                 accept "Event" -# number # parseEvents >->
                 (\(n, e) -> (Event n, TileItem "" e))

parseWorldItems :: Parser World
parseWorldItems = parseWorldItem # parseWorldItems >->
                  prepend ! Parser.return []

oneStartEnd :: World -> Bool
oneStartEnd world = count == (1, 1)
  where
    world' = map snd $ filter ((eqTile (Map 1)) . fst) world
    count = foldl foldCount (0, 0) world'
    foldCount (s, e) (MapContent (_, tiles)) = (s + starts, e + ends)
      where
        starts = lengthFilter Start
        ends = lengthFilter End
        lengthFilter = length . flip filter tiles . (==)
    foldCount (_, _) (TileItem _ _) = error "Should never happen"

formatWorld :: Maybe (World, String) -> Maybe World
formatWorld Nothing = Nothing
formatWorld (Just (worldItems, ""))
  | oneStartEnd worldItems = Just worldItems
  | otherwise = Nothing
formatWorld (Just (_, _)) = Nothing -- Garbage parsing error

parseWorld :: String -> Maybe World
parseWorld = formatWorld . parseWorldItems
-----------------------------------------------------------------------
