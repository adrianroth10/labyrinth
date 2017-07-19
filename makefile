VPATH = app/ src/

.PHONY: all run clean
MAIN = Main.hs
MODULES = Map.hs Graphics.hs CoreParser.hs Parser.hs

all: bin/labyrinth.html run

bin/labyrinth.html: $(MAIN) $(MODULES)
	hastec --output-html --outdir=bin/ -o $@ $^

run: labyrinth.html
	firefox --new-tab bin/labyrinth.html

clean:
	rm -r bin/*.jsmod bin/labyrinth.html
