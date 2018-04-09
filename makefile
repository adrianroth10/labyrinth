VPATH = src/
.PHONY: all run clean

MAIN = example/Labyrinth.hs

all: run

labyrinth.js: $(MAIN) Game.hs Graphics.hs World.hs Parser.hs
	hastec -Wall -fno-warn-unused-do-bind -o $@ $^

run: labyrinth.js
	cp $< example/
	xdg-open example/index.html

clean:
	rm -f *.jsmod *.js
