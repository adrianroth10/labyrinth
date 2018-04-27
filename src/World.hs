{-# LANGUAGE OverloadedStrings #-}
module World (World,
              Tile (Free, Start, Wall, Event, Map, Player, Checkpoint, NoTile),
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
            Checkpoint Integer |
            NoTile deriving (Eq, Show)
data TileItem = TileItem String EventItem |
                MapItem (Double, [Tile]) |
                PlayerItem String String Moves |
                CheckpointItem World EventItem
                deriving (Eq, Show)
type MapItem' = (Double, [Tile])
type Moves = [(String, Double, EventItem)]
data EventItem = NoEvent |
                 Text String |
                 FullText String String |
                 HTMLText String |
                 Teleport Tile Point |
                 Fight EventItem (Tile, Tile) (EventItem, EventItem) |
                 IncrementCheckpoints Tile World |
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
type Error = String
infixl 4 <:>
errorSeparation :: String
errorSeparation = "</br>"

(<:>) :: Either Error (a -> b) -> Either Error a -> Either Error b
(<:>) (Left a) (Left b) = Left (a ++ errorSeparation ++ b)
(<:>) (Right _) (Left b) = Left b
(<:>) (Left a) (Right _) = Left a
(<:>) (Right f) (Right b) = Right (f b)

infixl 1 =<<<
(=<<<) :: (a -> b -> Either Error c) -> Either Error a -> Either Error b
      -> Either Error c
(=<<<) _ (Left a) (Left b) = Left (a ++ errorSeparation ++ b)
(=<<<) _ (Right _) (Left b) = Left b
(=<<<) _ (Left a) (Right _) = Left a
(=<<<) f (Right a) (Right b) = f a b

eitherOut :: [Either Error a] -> Either Error [a]
eitherOut = foldr ((<:>) . ((:) <$>)) (Right [])
-------------------------------------------------------------------------

----------------------------------JSON---------------------------------
tryExtract :: JSString -> JSON -> Either Error JSON
tryExtract l (Dict a) = maybe (Left (fromJSStr l ++ " not found."))
                              Right (lookup l a)
tryExtract _ j = Left (show j ++ " must be an object.")

optTryExtract :: JSString -> JSON -> JSON -> Either Error JSON
optTryExtract k d (Dict a) = maybe (Right d) Right (lookup k a)
optTryExtract _ _ j = Left (show j ++ " must be an object.")

double :: JSON -> Either Error Double
double Null = Right 0
double (Num d) = Right d
double j = Left ("Error converting " ++ show j ++ " to double.")

integer :: JSON -> Either Error Integer
integer = fmap floor . double

string :: JSON -> Either Error String
string Null = Right ""
string (Str s) = Right (fromJSStr s)
string j = Left ("Error converting " ++ show j ++ " to string.")

array :: (JSON -> Either Error b) -> JSON -> Either Error [b]
array _ Null = Right []
array f (Arr a) = eitherOut (map f a)
array _ j = Left ("Error converting " ++ show j ++ " to array.")
-----------------------------------------------------------------------

--------------------------------Events---------------------------------
eventItem :: Tile -> JSON -> Either Error EventItem
eventItem _ (Dict [("Text", s)]) = Text <$> string s
eventItem _ (Dict [("FullText", Arr [s1, s2])]) =
    FullText <$> string s1 <:>
                 string s2
eventItem _ (Dict [("FullText", j)]) =
    Left ("Event key FullText should have the value"
       ++ " of an array of two strings as `[s1, s2] not "
       ++ show j ++ ".")
eventItem _ (Dict [("HTMLText", s)]) = HTMLText <$> string s
eventItem _ (Dict [("Teleport", Dict [("Map", n), ("Point", Arr [x, y])])]) =
    Teleport <$> tile "Map" n <:>
        (tuple2 <$> double x <:> double y)
eventItem _ (Dict [("Teleport", j)]) =
    Left ("Event key Teleport should have the value"
       ++ " of an object with a Map tile and a point (x, y) not "
       ++ show j ++ ".")
eventItem t (Dict [("Fight", Dict [("Events", Arr [eIntro, eWin, eLose]),
                         ("Players", Arr [p1, p2])])]) =
    Fight <$> events t eIntro <:>
        (tuple2 <$> tile "Player" p1 <:> tile "Player" p2) <:>
        (tuple2 <$> events t eWin <:> events t eLose)
eventItem _ (Dict [("Fight", j)]) =
    Left ("Event key Fight should have the value"
       ++ " of an object with a key Events which is an array of"
       ++ " three event objects [eIntro, eWin, eLose], the key Players with"
       ++ " the value of an array of integers [p1, p2] of which players"
       ++ " should be fighting not the recieved: "
       ++ show j ++ ".")
eventItem t (Dict [("Checkpoint", w)]) = IncrementCheckpoints t <$> world w
eventItem _ (Dict [("SetCheckpoint", n)]) = SetCheckpoint <$> integer n
eventItem _ (Dict [(j, _)]) = Left ("EventItem " ++ show j ++ " not found.")
eventItem _ j = Left ("EventItem " ++ show j ++ " must be an object"
                     ++ " with only one entry.")

formatEvents :: [EventItem] -> EventItem
formatEvents [] = NoEvent
formatEvents xs = EventItemList xs

events :: Tile -> JSON -> Either Error EventItem
events _ Null = Right NoEvent
events t (Arr es) = formatEvents <$> array (eventItem t) (Arr es)
events t (Dict e) = (EventItemList . return) <$> eventItem t (Dict e)
events _ j = Left (show j ++ " must be either an array or object.")
-----------------------------------------------------------------------

---------------------------------Moves---------------------------------
move :: Tile -> JSON -> Either Error (String, Double, EventItem)
move t (Arr [name, damage, es]) = (\ a b c -> (a, b, c)) <$>
        string name <:> double damage <:> events t es
move _ j = Left (show j ++ " could not be converted to player-moves," ++
               " should be on the form ´[name, damage, events]´.")

moves :: Tile -> JSON -> Either Error Moves
moves = array . move
-----------------------------------------------------------------------

-----------------------------------Map---------------------------------
mapTile' :: Integer -> Either Error Tile
mapTile' n
  | n == 0 = Right Start
  | n >= 10 && n < 20 = Right (Free (n - 9))
  | n >= 20 && n < 30 = Right (Wall (n - 19))
  | n >= 30 = Right (Event (n - 29))
  | otherwise = Left (show n ++ " is not a valid map tile number.")

mapTile :: JSON -> Either Error Tile
mapTile = (mapTile' =<<) . integer

mapContent :: JSON -> Either Error TileItem
mapContent (Arr [columns, content]) = MapItem <$>
        (tuple2 <$> double columns <:> array mapTile content)
mapContent j = Left (show j ++ " does not match the [double, [double]]"
                     ++ " format needed for the MapItem.")
-----------------------------------------------------------------------

-------------------------------Validate--------------------------------
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
validateEvents w (IncrementCheckpoints _ w') =
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

validateWorld :: World -> Either Error World
validateWorld w = maybe (Right w) Left $
                                oneStart w <**>
                                errorMap (validateWorldItem w) w
-----------------------------------------------------------------------

----------------------------------World--------------------------------
tileNumber' :: Integer -> Either Error Integer
tileNumber' n
  | n > 0 = Right n
  | otherwise = Left ("Tile number " ++ show n ++
                      " must be positive.")
tileNumber :: JSON -> Either Error Integer
tileNumber = (tileNumber' =<<) . integer

tile :: String -> JSON -> Either Error Tile
tile "Start" = const (Right Start)
tile "Free" = fmap Free . tileNumber
tile "Wall" = fmap Wall . tileNumber
tile "Event" = fmap Event . tileNumber
tile "Map" = fmap Map . tileNumber
tile "Player" = fmap Player . tileNumber
tile "Checkpoint" = fmap Checkpoint . tileNumber
tile s = const (Left ("Tile " ++ show s ++ " not found."))

worldItem :: JSON -> Either Error WorldItem
worldItem (Dict (("Map", n):xs)) = tuple2 <$>
        tile "Map" n <:>
        (tryExtract "MapItem" (Dict xs) >>= mapContent)
worldItem (Dict (("Player", n):xs)) = tuple2 <$>
        t <:>
        (PlayerItem <$>
         (tryExtract "Image" (Dict xs) >>= string) <:>
         (tryExtract "Name" (Dict xs) >>= string) <:>
         (moves =<<< t) (tryExtract "Moves" (Dict xs)))
          where t = tile "Player" n
worldItem (Dict (("Checkpoint", n):xs)) = tuple2 <$>
        t <:>
        (CheckpointItem <$>
         (optTryExtract "World" Null (Dict xs) >>= world) <:>
         (events =<<< t) (optTryExtract "Events" Null (Dict xs)))
          where t = tile "Checkpoint" n
worldItem (Dict ((s, n):xs)) = tuple2 <$>
        t <:>
        (TileItem <$>
         (optTryExtract "Image" Null (Dict xs) >>= string) <:>
         (events =<<< t) (optTryExtract "Events" Null (Dict xs)))
          where t = tile (fromJSStr s) n
worldItem j = Left (show j ++ "must be object.")

world :: JSON -> Either Error World
world = array worldItem

parseWorld :: String -> Either Error World
parseWorld = (validateWorld =<<) . (world =<<) . decodeJSON . toJSStr
-----------------------------------------------------------------------
