SHELL = /bin/sh

.PHONY: build clean doc default

default: build

../wasmdoc:
	mkdir ../wasmdoc; \
	cd ../wasmdoc; \
	git clone https://github.com/WebAssembly/design.git;

doc: ../wasmdoc
	cd ../wasmdoc/design; \
	git pull

build:
	ocamlbuild -use-menhir main.native

clean:
	ocamlbuild -clean
