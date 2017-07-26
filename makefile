.PHONY: all run clean
MAIN = Main.hs

all: run

labyrinth.js: Labyrinth.hs Graphics.hs Map.hs Game.hs
	hastec -Wall -fno-warn-unused-do-bind -o $@ $^

run: labyrinth.js
	firefox --new-tab index.html

clean:
	rm *.hi *.o *.jsmod *.js
