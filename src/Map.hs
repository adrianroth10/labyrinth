module Map (Map,
            Tile (Free, Start, End, Wall, Event),
            unTile,
            parseMap) where

import Parser
import Data.Maybe

type Map = (Double, [Tile])
data Tile = Free | Start String | End String |
            Wall | Event String deriving (Eq, Show)

prepend :: (a, [a]) -> [a]
prepend (x, xs) = x:xs

eventContent :: Parser [String]
eventContent = accept "End" -# Parser.return [] !
               line # eventContent >-> prepend

parseEvent :: Parser (String, String)
parseEvent = accept "Event" -# (number >-> show) # (eventContent >-> unlines) !
             accept "Event" -# word # (eventContent >-> unlines)

parseEvents :: Parser [(String, String)]
parseEvents = parseEvent # parseEvents >-> prepend !  Parser.return []

inputEvents :: Maybe [(String, String)] -> [Tile] -> Maybe [Tile]
inputEvents Nothing _ = Nothing
inputEvents _ [] = Just []
inputEvents (Just []) tiles = Just tiles
inputEvents (Just events) ((Start _):ts)
  | isNothing event = fmap (Start "" :)
                     (inputEvents (Just events) ts)
  | otherwise = fmap (Start (fromJust event) :)
                     (inputEvents (Just events) ts)
  where event = lookup "Start" events
inputEvents (Just events) ((End _):ts)
  | isNothing event = fmap (End "" :)
                     (inputEvents (Just events) ts)
  | otherwise = fmap (End (fromJust event) :)
                     (inputEvents (Just events) ts)
  where event = lookup "End" events
inputEvents (Just events) ((Event s):ts)
  | isNothing event = Nothing
  | otherwise = fmap (Event (fromJust event) :)
                     (inputEvents (Just events) ts)
  where event = lookup s events
inputEvents events (t:ts) = fmap (t:) (inputEvents events ts)

tile :: Integer -> Tile
tile 0 = Free
tile 1 = Start ""
tile 2 = End ""
tile 3 = Wall
tile n = Event $ show $ n - 3

unTile :: Tile -> Integer
unTile Free = 0
unTile (Start _) = 1
unTile (End _) = 2
unTile Wall = 3
unTile (Event _) = 4

mapContent :: Parser [Integer]
mapContent = (number # mapContent >-> prepend) ! Parser.return []

format :: Maybe (((Integer, Integer), [Tile]), String) -> Maybe Map
format Nothing = Nothing
format (Just (((r, c), xs), s))
  | fromInteger (r * c) /= length xs = Nothing
  | length starts /= 1 = Nothing
  | length ends /= 1 = Nothing
  | otherwise = fmap (\tiles -> (fromInteger c, tiles)) eventedTiles
  where
    starts = filter ((==) 1 . unTile) xs
    ends = filter ((==) 2 . unTile) xs
    events = fmap fst $ parseEvents s
    eventedTiles = inputEvents events xs

parseMap :: String -> Maybe Map
parseMap = format . (number # number # (mapContent >-> (map tile)))

--loadMap :: String -> IO (Maybe Map)
--loadMap file = do
--  mapStr <- readFile file
--  Prelude.return $! parseMap mapStr

--testIn = "3\n3\n0 1 2\n3 4 4\n4 4 4\nEvent Start\nbla\nEnd\nEvent End\nha\nEnd\nEvent 1\n<p>\nHello World!\n</p>\nEnd"
--a = fromJust $ (number # number # (mapContent >-> (map tile))) $ parseMapIn1
--m = fst a
--t = snd m
--e = fmap fst $ parseEvents $ snd a
