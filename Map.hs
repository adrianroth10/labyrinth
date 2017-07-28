module Map (Map,
            Tile(Free, Start, End, Wall, Event)) where

type Map = (Double, [Tile])
data Tile = Free | Start | End | Wall | Event String deriving (Eq, Show)

