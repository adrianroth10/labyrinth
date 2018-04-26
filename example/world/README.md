# Format of world.json file
I have tried to make it as easy and intuitive as possible to make your own world.

The world consists of `[WorldItem]` where each `WorldItem` is a `(Tile, TileItem)`.
There are 6 different tiles:
```Haskell
data Tile = Start | Free Integer |
            Wall Integer | Event Integer |
            Map Integer | Player Integer deriving (Eq, Show)
```
Then each tile has a corresponding `TileItem`:
```Haskell
data TileItem = TileItem String EventItem |
		MapItem (Double, [Tile]) |
                PlayerItem String String Moves deriving (Eq, Show)
```
`TileItem` is for the first 4 tiles and the others are self explanatory.

A `WorldItem` for the tiles with `TileItem` `TileItem` is defined as one `JSON` object as follows (within parenthesis means optional):
```
{
	"Tile" : n (Integer),
	("Image" : "Path"),
	("Events" : { [EventItems] })
}
```

A `WorldItem` for the tiles with `TileItem` `MapItem` is defined as:
```
{
	"Map" : n (Integer),
	"MapItem" : 
	[
		Columns (Integer),
		Content ([Integer])
	]
}
```
The content integers (`n`) will correspond to tiles as:
```Haskell
mapTile' :: Integer -> Either String Tile
mapTile' n
  | n == 0 = Right Start
  | n >= 10 && n < 20 = Right (Free (n - 9))
  | n >= 20 && n < 30 = Right (Wall (n - 19))
  | n >= 30 = Right (Event (n - 29))
  | otherwise = Left (show n ++ " is not a valid map tile number.")
```

A `WorldItem` for the tiles with `TileItem` `PlayerItem` is defined as:
```
{
	"Player" : n (Integer),
	"Image" : "Path",
	"Name" : "String",
	"Moves" : [Move]
}
```
One move is a JSON object as:
```
{
	"Name" : "String",
	"Damage" : d (Double),
	"Events" : [EventItem]
}
```
The player 1 in the game will correspond to the player you are able to control.

At last the `EventItems` have been mentioned, they are defined by 
```Haskell
data EventItem = NoEvent |
                 Text String |
                 FullText String String |
                 HTMLText String |
                 Teleport Tile Point |
                 Fight EventItem (Tile, Tile) (EventItem, EventItem) |
```
These events are mostly inspired by Pokèmon: since `Text` event is just text at the bottom, `FullText` looks like sliding credits, `HTMLText` is the possibility to input HTML code in a div element with id "output" in the HTML file running the Javascript, `Teleport` can move the player between different maps, and `Fight` is more or less a Pokèmon fight between two player tiles. They are defined in the world file as either a list of `EventItem`s or just one. An example of each event is shown below. `null` is the `NoEvent`:
```
[
	null,
	{ "Text" : "String", },
	{ "FullText" : ["String" (title), "String"] },
	{ "HTMLText" : String },
	{ "Teleport" : { "Map" : n, "Point" [x (Integer),y (Integer)] } },
	{ 
		"Fight" :
		{
			[
				[Eventitem] (Intro events),
				[Eventitem] (Win events),
				[Eventitem] (Lose events)
			]
		}
		"Players" : [Player1 (Integer), Player2 (Integer)]
	}
]
```
