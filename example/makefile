VPATH = ../src/
.PHONY: all run clean
MAIN = Main.hs

all: run

labyrinth.js: Labyrinth.hs Game.hs Graphics.hs World.hs Parser.hs
	hastec -Wall -fno-warn-unused-do-bind -o $@ $^

run: labyrinth.js
	firefox index.html

clean:
	rm -f *.hi *.o *.jsmod *.js
