VPATH = ../src/
.PHONY: all run clean

all: run

labyrinth.js: Labyrinth.hs Game.hs Graphics.hs World.hs Parser.hs
	hastec -Wall -fno-warn-unused-do-bind -o $@ $^

run: labyrinth.js
	xdg-open index.html

clean:
	rm -f *.hi *.o *.jsmod *.js
