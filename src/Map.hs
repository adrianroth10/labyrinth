module Map where --(Map,
            --Tile (Free, Start, End, Wall, Event),
            --unTile,
            --parseMap) where

import Parser
import Data.Maybe

type Map = (Double, [Tile])
-- First string is always img path and second is the event on the tile
data Tile = Start String String | End String String | Free String |
            Wall String | Event String String deriving (Eq, Show)

prepend :: (a, [a]) -> [a]
prepend (x, xs) = x:xs

eventContent :: Parser [String]
eventContent = accept "END" -# Parser.return [] !
               line # eventContent >-> prepend

parseEvent :: Parser (Integer, (String, String))
parseEvent = accept "Start" -# accept "IMG" -# (var # (eventContent >-> unlines)) >-> (\(img, event) -> (unTile (Start "" ""), (img, event))) !
             accept "Start" -# (eventContent >-> unlines) >-> (\event -> (unTile (Start "" ""), ("", event))) !
             accept "End" -# accept "IMG" -# (var # (eventContent >-> unlines)) >-> (\(img, event) -> (unTile (End "" ""), (img, event))) !
             accept "End" -# (eventContent >-> unlines) >-> (\event -> (unTile (End "" ""), ("", event))) !
             accept "Free" -# (number #- accept "IMG" # var) >-> (\(n, img) -> (n + unTile (Free ""), (img, ""))) !
             accept "Free" -# number >-> (\n -> (n + unTile (Free ""), ("", ""))) !
             accept "Wall" -# (number #- accept "IMG" # var) >-> (\(n, img) -> (n + unTile (Wall ""), (img, ""))) !
             accept "Wall" -# number >-> (\n -> (n + unTile (Wall ""), ("", ""))) !
             accept "Event" -# (number #- accept "IMG" # var # (eventContent >-> unlines)) >-> (\((n, img), event) -> (n + unTile (Event "" ""), (img, event))) !
             accept "Event" -# (number # (eventContent >-> unlines)) >-> (\(n, event) -> (n + unTile (Event "" ""), ("", event)))

parseEvents :: Parser [(Integer, (String, String))]
parseEvents = parseEvent # parseEvents >-> prepend !  Parser.return []

inputEvents :: Maybe [(Integer, (String, String))] -> [Integer] -> Maybe [Tile]
inputEvents Nothing _ = Nothing
inputEvents _ [] = Just []
inputEvents (Just []) tiles = Just $ map (flip tile ("", "")) tiles
inputEvents (Just events) (n:ns) = fmap (maybe (tile n ("", "")) (tile n) (lookup n events):) (inputEvents (Just events) ns)

tile :: Integer -> (String, String) -> Tile
tile n (img, event)
  | n == unTile (Start "" "") = Start img event
  | n == unTile (End "" "") = End img event
  | n >= unTile (Free "") || n < unTile (Wall "") = Free img
  | n >= unTile (Wall "") || n < unTile (Event "" "") = Wall img
  | n >= unTile (Event "" "") = Event img event

unTile :: Tile -> Integer
unTile (Start _ _) = 0
unTile (End _ _) = 1
unTile (Free _) = 10
unTile (Wall _) = 20
unTile (Event _ _) = 30

format :: Maybe (((Integer, Integer), [Integer]), String) -> Maybe Map
format Nothing = Nothing
format (Just (((r, c), xs), s))
  | fromInteger (r * c) /= length xs = Nothing
  | length starts /= 1 = Nothing
  | length ends /= 1 = Nothing
  | otherwise = fmap (\tiles -> (fromInteger c, tiles)) eventedTiles
  where
    starts = filter ((==) 0) xs
    ends = filter ((==) 1) xs
    events = fmap fst $ parseEvents s
    eventedTiles = inputEvents events xs

mapContent :: Parser [Integer]
mapContent = (number # mapContent >-> prepend) ! Parser.return []

parseMap :: String -> Maybe Map
parseMap = format . (number # number # mapContent)
