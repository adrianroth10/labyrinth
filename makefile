.PHONY: all run clean
MAIN = Main.hs

all: run

labyrinth.js: labyrinth.hs
	hastec -o $@ $^

run: labyrinth.js
	firefox --new-tab index.html

clean:
	rm -r *.hi *.ho *.js
