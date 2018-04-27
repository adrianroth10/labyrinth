{-# LANGUAGE OverloadedStrings #-}
module World (World,
              Tile (Free, Start, Wall, Event, Map, Player, Checkpoint),
              TileItem (MapItem, TileItem, PlayerItem, CheckpointItem),
              MapItem',
              Moves,
              EventItem (NoEvent, Locked, Text, FullText,
                         HTMLText, Teleport, Fight, Animation,
                         ChangePoint, EventItemList,
                         IncrementCheckpoints, SetCheckpoint),
              AnimationInfo (AnimationInfo),
              eqTile,
              parseWorld) where

import Haste.Prim
import Haste.JSON


type Point = (Double, Double)
type World = [WorldItem]
type WorldItem = (Tile, TileItem)
data Tile = Start | Free Integer |
            Wall Integer | Event Integer |
            Map Integer | Player Integer |
            Checkpoint Integer deriving (Eq, Show)
data TileItem = TileItem String EventItem |
                MapItem (Double, [Tile]) |
                PlayerItem String String Moves |
                CheckpointItem World EventItem deriving (Eq, Show)
type MapItem' = (Double, [Tile])
type Moves = [(String, Double, EventItem)]
data EventItem = NoEvent |
                 Text String |
                 FullText String String |
                 HTMLText String |
                 Teleport Tile Point |
                 Fight EventItem (Tile, Tile) (EventItem, EventItem) |
                 IncrementCheckpoints World |
                 SetCheckpoint Integer |
                 -- Non parsable events ->
                 Locked |
                 Animation AnimationInfo |
                 ChangePoint Point  |
                 EventItemList [EventItem] deriving (Eq, Show)

data AnimationInfo = AnimationInfo [(Point -> IO (), [Point])]
                                   EventItem
instance Show AnimationInfo where
  show = const ""
instance Eq AnimationInfo where
  (==) _ _ = True


eqTile :: Tile -> Tile -> Bool
eqTile Start Start = True
eqTile (Free _) (Free _) = True
eqTile (Wall _) (Wall _) = True
eqTile (Event _) (Event _) = True
eqTile (Map _) (Map _) = True
eqTile (Player _) (Player _) = True
eqTile _ _ = False

tuple2 :: a -> b -> (a, b)
tuple2 a b = (a, b)

-----------------------------------Either---------------------------------
infixl 4 <:>
(<:>) :: Either String (a -> b) -> Either String a -> Either String b
(<:>) (Left a) (Left b) = Left (a ++ "</br>" ++ b)
(<:>) (Right _) (Left b) = Left b
(<:>) (Left a) (Right _) = Left a
(<:>) (Right f) (Right b) = Right (f b)

eitherOut :: [Either String a] -> Either String [a]
eitherOut = foldr ((<:>) . ((:) <$>)) (Right [])
-------------------------------------------------------------------------

----------------------------------JSON---------------------------------
tryExtract :: JSString -> JSON -> Either String JSON
tryExtract l (Dict a) = maybe (Left (fromJSStr l ++ " not found."))
                              Right (lookup l a)
tryExtract _ j = Left (show j ++ " must be an object.")

optTryExtract :: JSString -> JSON -> JSON -> Either String JSON
optTryExtract k d (Dict a) = maybe (Right d) Right (lookup k a)
optTryExtract _ _ j = Left (show j ++ " must be an object.")

double :: JSON -> Either String Double
double Null = Right 0
double (Num d) = Right d
double j = Left ("Error converting " ++ show j ++ " to double.")

integer :: JSON -> Either String Integer
integer = fmap floor . double

string :: JSON -> Either String String
string Null = Right ""
string (Str s) = Right (fromJSStr s)
string j = Left ("Error converting " ++ show j ++ " to string.")

array :: (JSON -> Either String b) -> JSON -> Either String [b]
array _ Null = Right []
array f (Arr a) = eitherOut (map f a)
array _ j = Left ("Error converting " ++ show j ++ " to array.")
-----------------------------------------------------------------------

--------------------------------Events---------------------------------
eventItem :: JSON -> Either String EventItem
eventItem (Dict [("Text", s)]) = fmap Text (string s)
eventItem (Dict [("FullText", Arr [s1, s2])]) =
    FullText <$> string s1 <:>
                 string s2
eventItem (Dict [("FullText", j)]) =
    Left ("Event key FullText should have the value"
       ++ " of an array of two strings as `[s1, s2] not "
       ++ show j ++ ".")
eventItem (Dict [("HTMLText", s)]) = fmap HTMLText (string s)
eventItem (Dict [("Teleport", Dict [("Map", n), ("Point", Arr [x, y])])]) =
    Teleport <$> tile "Map" n <:>
        (tuple2 <$> double x <:> double y)
eventItem (Dict [("Teleport", j)]) =
    Left ("Event key Teleport should have the value"
       ++ " of an object with a Map tile and a point (x, y) not "
       ++ show j ++ ".")
eventItem (Dict [("Fight", Dict [("Events", Arr [eIntro, eWin, eLose]),
                         ("Players", Arr [p1, p2])])]) =
    Fight <$> events eIntro <:>
        (tuple2 <$> tile "Player" p1 <:> tile "Player" p2) <:>
        (tuple2 <$> events eWin <:> events eLose)
eventItem (Dict [("Fight", j)]) =
    Left ("Event key Fight should have the value"
       ++ " of an object with a key Events which is an array of"
       ++ " three event objects [eIntro, eWin, eLose], the key Players with"
       ++ " the value of an array of integers [p1, p2] of which players"
       ++ " should be fighting not the recieved: "
       ++ show j ++ ".")
eventItem (Dict [("Checkpoint", w)]) = fmap IncrementCheckpoints (world w)
eventItem (Dict [("SetCheckpoint", n)]) = fmap SetCheckpoint (integer n)
eventItem (Dict [(j, _)]) = Left ("EventItem " ++ show j ++ " not found.")
eventItem j = Left ("EventItem " ++ show j ++ " must be an object"
                     ++ " with only one entry.")

formatEvents :: [EventItem] -> EventItem
formatEvents [] = NoEvent
formatEvents xs = EventItemList xs

events :: JSON -> Either String EventItem
events Null = Right NoEvent
events (Arr es) = fmap formatEvents (array eventItem (Arr es))
events (Dict e) = fmap (EventItemList . return) (eventItem (Dict e))
events j = Left (show j ++ " must be either an array or object.")
-----------------------------------------------------------------------

---------------------------------Moves---------------------------------
move :: JSON -> Either String (String, Double, EventItem)
move (Arr [name, damage, es]) = (\ a b c -> (a, b, c)) <$>
        string name <:> double damage <:> events es
move j = Left (show j ++ " could not be converted to player-moves," ++
               " should be on the form ´[name, damage, events]´.")

moves :: JSON -> Either String Moves
moves = array move
-----------------------------------------------------------------------

-----------------------------------Map---------------------------------
mapTile' :: Integer -> Either String Tile
mapTile' n
  | n == 0 = Right Start
  | n >= 10 && n < 20 = Right (Free (n - 9))
  | n >= 20 && n < 30 = Right (Wall (n - 19))
  | n >= 30 = Right (Event (n - 29))
  | otherwise = Left (show n ++ " is not a valid map tile number.")

mapTile :: JSON -> Either String Tile
mapTile = (mapTile' =<<) . integer

mapContent :: JSON -> Either String TileItem
mapContent (Arr [columns, content]) = MapItem <$>
        (tuple2 <$> double columns <:> array mapTile content)
mapContent j = Left (show j ++ " does not match the [double, [double]]"
                     ++ " format needed for the MapItem.")
-----------------------------------------------------------------------

-------------------------------Validate--------------------------------
type Error = String
infixl 4 <**>
(<**>) :: Maybe Error -> Maybe Error -> Maybe Error
(<**>) (Just a) (Just b) = Just (a ++ "</br>" ++ b)
(<**>) (Just a) Nothing = Just a
(<**>) Nothing (Just b) = Just b
(<**>) Nothing Nothing = Nothing

errorMap :: (a -> Maybe Error) -> [a] -> Maybe Error
errorMap _ [] = Nothing
errorMap f (x:xs) = f x <**> errorMap f xs


oneStart :: World -> Maybe Error
oneStart w
  | count == 1 = Nothing
  | otherwise = Just ("World maps should have exactly 1 Start tile, " ++
                      show count ++ " found.")
  where
    world' = map snd $ filter (eqTile (Map 1) . fst) w
    count = foldl foldCount 0 world'
    foldCount s (MapItem (_, tiles)) = s + starts
      where
        starts = lengthFilter Start
        lengthFilter = length . flip filter tiles . (==)
    foldCount _ _ = error "Should never happen"

validateTile :: World -> Tile -> Maybe Error
validateTile w t = maybe (Just ("Tile `" ++ show t ++
                                "` used but not defined."))
                         (const Nothing) (lookup t w)

validateEvents :: World -> EventItem -> Maybe Error
validateEvents w (Teleport t _) = validateTile w t
validateEvents w (Fight ei (p1, p2) (ew, el)) =
    validateTile w p1 <**> validateTile w p2 <**>
    validateEvents w (EventItemList [ei, ew, el])
validateEvents w (IncrementCheckpoints w') =
                                errorMap (validateTile (w ++ w') . fst) w'
validateEvents w (EventItemList xs) = errorMap (validateEvents w) xs
validateEvents _ _ = Nothing

validateMove :: World -> (String, Double, EventItem) -> Maybe Error
validateMove w (_, _, e) = validateEvents w e
validateMoves :: World -> Moves -> Maybe Error
validateMoves w = errorMap (validateMove w)


squareMap :: MapItem' -> Maybe Error
squareMap (c, tiles)
  | length tiles `mod` floor c == 0 = Nothing
  | otherwise = Just ("MapItem " ++ show (c, tiles) ++
                      " should fulfill `length [tiles] % columns == 0`.")
validateMap :: World -> MapItem' -> Maybe Error
validateMap w (d, c) = squareMap (d, c) <**> errorMap (validateTile w) c

validateWorldItem :: World -> WorldItem -> Maybe Error
validateWorldItem w (Map _, MapItem mc) = validateMap w mc
validateWorldItem w (Player _, PlayerItem _ _ m) = validateMoves w m
validateWorldItem w (Checkpoint _, CheckpointItem w' es) =
                                    errorMap (validateTile (w ++ w') . fst) w'
                               <**> validateEvents w es
validateWorldItem w (_, TileItem _ e) = validateEvents w e
validateWorldItem _ _ = Nothing

validateWorld :: World -> Either String World
validateWorld w = maybe (Right w) Left $
                                oneStart w <**>
                                errorMap (validateWorldItem w) w
-----------------------------------------------------------------------

----------------------------------World--------------------------------
tileNumber' :: Integer -> Either String Integer
tileNumber' n
  | n > 0 = Right n
  | otherwise = Left ("Tile number " ++ show n ++
                      " must be positive.")
tileNumber :: JSON -> Either String Integer
tileNumber = (tileNumber' =<<) . integer

tile :: String -> JSON -> Either String Tile
tile "Start" = const (Right Start)
tile "Free" = fmap Free . tileNumber
tile "Wall" = fmap Wall . tileNumber
tile "Event" = fmap Event . tileNumber
tile "Map" = fmap Map . tileNumber
tile "Player" = fmap Player . tileNumber
tile "Checkpoint" = fmap Checkpoint . tileNumber
tile s = const (Left ("Tile " ++ show s ++ " not found."))

worldItem :: JSON -> Either String WorldItem
worldItem (Dict (("Map", n):xs)) = tuple2 <$>
        tile "Map" n <:>
        (tryExtract "MapItem" (Dict xs) >>= mapContent)
worldItem (Dict (("Player", n):xs)) = tuple2 <$>
        tile "Player" n <:>
        (PlayerItem <$>
         (tryExtract "Image" (Dict xs) >>= string) <:>
         (tryExtract "Name" (Dict xs) >>= string) <:>
         (tryExtract "Moves" (Dict xs) >>= moves))
worldItem (Dict (("Checkpoint", n):xs)) = tuple2 <$>
        tile "Checkpoint" n <:>
        (CheckpointItem <$>
        (optTryExtract "World" Null (Dict xs) >>= world) <:>
        (optTryExtract "Events" Null (Dict xs) >>= events))
worldItem (Dict ((s, n):xs)) = tuple2 <$>
        tile (fromJSStr s) n <:>
        (TileItem <$>
         (optTryExtract "Image" Null (Dict xs) >>= string) <:>
         (optTryExtract "Events" Null (Dict xs) >>= events))
worldItem j = Left (show j ++ "must be object.")

world :: JSON -> Either String World
world = array worldItem

parseWorld :: String -> Either String World
parseWorld = (validateWorld =<<) . (world =<<) . decodeJSON . toJSStr
-----------------------------------------------------------------------
