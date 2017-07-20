VPATH = app/ src/ test/

.PHONY: all run test stackTest clean
MAIN = Main.hs
MODULES = Map.hs Graphics.hs CoreParser.hs Parser.hs

all: run

bin/labyrinth.html: $(MAIN) $(MODULES)
	hastec --output-html --outdir=bin/ -o $@ $^

run: bin/labyrinth.html
	firefox --new-tab $<

test: stackTest bin/graphicsSpec.html

stackTest:
	stack test

bin/graphicsSpec.html: GraphicsSpec.hs $(MODULES)
	hastec --output-html --outdir=bin/ -o $@ $^
	firefox --new-tab $@

clean:
	rm -r bin/*.jsmod bin/labyrinth.html
