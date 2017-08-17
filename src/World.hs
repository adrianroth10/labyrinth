module World (World,
            Tile (Free, Start, Wall, Event, Map, Player),
            TileItem (MapContent, TileItem, PlayerItem),
            MapContent',
            EventItem (NoEvent, Locked, Text, HTMLText,
                       Teleport, EventItemList),
            eqTile,
            parseWorld) where

import Parser

type World = [(Tile, TileItem)]
data Tile = Start | Free Integer |
            Wall Integer | Event Integer |
            Map Integer | Player Integer deriving (Eq, Show)
data TileItem = MapContent (Double, [Tile]) |
                TileItem String EventItem |
                PlayerItem String deriving (Eq, Show)
type MapContent' = (Double, [Tile])
data EventItem = NoEvent |
                 Locked |
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
eqTile (Free _) (Free _) = True
eqTile (Wall _) (Wall _) = True
eqTile (Event _) (Event _) = True
eqTile (Map _) (Map _) = True
eqTile (Player _) (Player _) = True
eqTile _ _ = False

tile :: Integer -> Tile
tile n
  | n == 0 = Start
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
                 (\(n, e) -> (Event n, TileItem "" e)) !

                 accept "Player" -# number #- accept img # var >->
                 (\(n, image) -> (Player n, PlayerItem image))

parseWorldItems :: Parser World
parseWorldItems = parseWorldItem # parseWorldItems >->
                  prepend ! Parser.return []

oneStart :: World -> Bool
oneStart world = count == 1
  where
    world' = map snd $ filter ((eqTile (Map 1)) . fst) world
    count = foldl foldCount 0 world'
    foldCount s (MapContent (_, tiles)) = s + starts
      where
        starts = lengthFilter Start
        lengthFilter = length . flip filter tiles . (==)
    foldCount _ (TileItem _ _) = error "Should never happen"
    foldCount _ (PlayerItem _) = error "Should never happen"

formatWorld :: Maybe (World, String) -> Maybe World
formatWorld Nothing = Nothing
formatWorld (Just (worldItems, ""))
  | oneStart worldItems = Just worldItems
  | otherwise = Nothing
formatWorld (Just (_, _)) = Nothing -- Garbage parsing error

parseWorld :: String -> Maybe World
parseWorld = formatWorld . parseWorldItems
-----------------------------------------------------------------------
