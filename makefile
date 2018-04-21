VPATH = src/
.PHONY: all run clean

MAIN = example/Labyrinth.hs
OUT = example/labyrinth.js

test.js: World.hs
	hastec -Wall -o $@ $^

$(OUT): $(MAIN) Game.hs Graphics.hs World.hs Parser.hs
	hastec -Wall -fno-warn-unused-do-bind -o $@ $^

run: $(OUT)
	xdg-open example/index.html

clean:
	rm -f *.jsmod $(OUT)
