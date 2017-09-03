module World (World,
              Tile (Free, Start, Wall, Event, Map, Player),
              TileItem (MapContent, TileItem, PlayerItem),
              MapContent',
              Moves,
              EventItem (NoEvent, Locked, Text, FullText,
                         HTMLText, Teleport, Fight, Animation,
                         ChangePoint, EventItemList),
              AnimationInfo (AnimationInfo),
              eqTile,
              parseWorld) where

import Parser

type Point = (Double, Double)
type World = [(Tile, TileItem)]
data Tile = Start | Free Integer |
            Wall Integer | Event Integer |
            Map Integer | Player Integer deriving (Eq, Show)
data TileItem = MapContent (Double, [Tile]) |
                TileItem String EventItem |
                PlayerItem String String Moves deriving (Eq, Show)
type MapContent' = (Double, [Tile])
type Moves = [(String, Double, EventItem)]
data EventItem = NoEvent |
                 Locked |
                 Text String |
                 FullText String String |
                 HTMLText String |
                 Teleport Tile Point |
                 Fight EventItem (Tile, Tile) (EventItem, EventItem) |
                 Animation AnimationInfo |
                 ChangePoint Point  |
                 EventItemList [EventItem] deriving (Eq, Show)

data AnimationInfo = AnimationInfo [(Point -> IO (), [Point])]
                                   EventItem
instance Show AnimationInfo where
  show = const "" 
instance Eq AnimationInfo where
  (==) _ _ = True

end, img, emptyLine :: String
end = "END"
img = "IMG"
emptyLine = "EMPTYLINE"

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

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace pat rep (x:xs)
  | x == pat = rep : replace pat rep xs
  | otherwise = x : replace pat rep xs


---------------------------------Event---------------------------------
parseLines :: Parser [String]
parseLines = accept end -# Parser.return [] !
               line # parseLines >-> prepend

parseEvent :: Parser EventItem
parseEvent = accept "Text" -# parseLines >->
             (Text . unlines . (replace emptyLine ""))
             !
             accept "FullText" -# parseLines >-> replace emptyLine ""
             >-> (\content -> FullText (head content)
                                       (unlines (tail content)))
             !
             accept "HTMLText" -# parseLines >->
             (HTMLText . unlines)
             !
             accept "Teleport" -# accept "Map" -# number #-
             accept "Point" # (number >-> fromInteger) # 
             (number >-> fromInteger) >->
             (\((n, x), y) -> Teleport (Map n) (x, y))
             !
             accept "Fight" -# accept "Start" -# parseEvents #-
             accept "Player" # number #- accept "Player" # number #-
             accept "Win" # parseEvents #-
             accept "Lose" # parseEvents #- accept end >->
             (\((((start, p1), p2), win), lose) ->
                        Fight start (Player p1, Player p2) (win, lose))

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

---------------------------------Moves---------------------------------
parseMove :: Parser (String, Double, EventItem)
parseMove = accept "Move" -# accept "Name" -# var #-
            accept "Damage" # number #-
            accept "Events" # parseEvents >->
            (\((name, damage), event) ->
                                    (name, fromIntegral damage, event))

parseMoves' :: Parser Moves
parseMoves' = accept end -# Parser.return [] !
               parseMove # parseMoves' >-> prepend
parseMoves :: Parser Moves
parseMoves = parseMoves' ? ((<=4) . length)
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
                 (\(n, m) -> (Map n, m))
                 !
                 accept "Start" -# accept img -# var # parseEvents >->
                 (\(image, e) -> (Start, TileItem image e)) !
                 accept "Start" -# parseEvents >->
                 (\e -> (Start, TileItem "" e))
                 !
                 accept "Free" -# number #- accept img # var >->
                 (\(n, image) -> (Free n, TileItem image NoEvent)) !
                 accept "Free" -# number >->
                 (\n -> (Free n, TileItem "" NoEvent))
                 !
                 accept "Wall" -# number #- accept img # var >->
                 (\(n, image) -> (Wall n, TileItem image NoEvent)) !
                 accept "Wall" -# number >->
                 (\n -> (Wall n, TileItem "" NoEvent))
                 !
                 accept "Event" -#
                 number #- accept img # var # parseEvents >->
                 (\((n, image), e) -> (Event n, TileItem image e)) !
                 accept "Event" -# number # parseEvents >->
                 (\(n, e) -> (Event n, TileItem "" e))
                 !
                 accept "Player" -# number #- accept img # var #-
                 accept "Name" # var #-
                 accept "Moves" # parseMoves #- accept end >->
                 (\(((n, image), name), moves) ->
                             (Player n, PlayerItem image name moves)) !
                 accept "Player" -# number #- accept img # var >->
                 (\(n, image) ->
                               (Player n, PlayerItem image "" []))

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
    foldCount _ (PlayerItem _ _ _) = error "Should never happen"

formatWorld :: Maybe (World, String) -> Maybe World
formatWorld Nothing = Nothing
formatWorld (Just (worldItems, ""))
  | oneStart worldItems = Just worldItems
  | otherwise = Nothing
formatWorld (Just (_, _)) = Nothing -- Garbage parsing error

parseWorld :: String -> Maybe World
parseWorld = formatWorld . parseWorldItems
-----------------------------------------------------------------------
