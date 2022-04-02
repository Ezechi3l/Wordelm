MANAGER = npm

.PHONY: dev clean elm

dev: node_modules
	make elm & $(MANAGER) run dev

build: node_modules
	make elm & $(MANAGER) run build

clean:
	rm -rf ./elm-stuff

node_modules: package.json
	$(MANAGER) install

elm: elm.json
	elm make

