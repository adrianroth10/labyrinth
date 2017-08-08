# Format of map file
I have tried to make it as easy and intuitive as possible to make your own map.

First row: number of rows in the labyrinth

Second row: number of columns in the labyrinth

Then follows a space and newline separated part with integral numbers describing the matrix that will be the labyrinth.
For example the start can look as follows for a labyrinth with 2 rows and 2 columns:

```
2
2
0 1
2 3
```

Each integral number describes a Tile where there are 5 different types which have corresponding different number(s):
  - Start 0
  - End 1
  - Free 10-19
  - Wall 20-29
  - Event 20-

The different tiles have different numbers for the possibility of different background images for the same type of tile.
This will hopefully give a cooler map!

After the matrix building up the map there are possibilities to input images for each tile number used in the matrix.
First the image paths are set up to sort of variables (only letters are allowed in the name) using the syntax
```
IMG VARNAME PATH
```

Then the images are connected to the tiles where Start, End and Event tiles also can add HTML code which will be displayed when the player stands on that tile.
This will have the syntax for example an event with NUMBER:
```
Event NUMBER IMG VARNAME
HTML_CODE
END
```

Otherwise the example map should contain all the functionality of the map parsing!
