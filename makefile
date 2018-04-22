VPATH = src/
.PHONY: all run clean

MAIN = example/Labyrinth.hs
OUT = example/labyrinth.js

$(OUT): $(MAIN) Game.hs Graphics.hs World.hs
	hastec -Wall -o $@ $^

run: $(OUT)
	xdg-open example/index.html

clean:
	rm -f *.jsmod $(OUT)
