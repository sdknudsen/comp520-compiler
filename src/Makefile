SHELL = /bin/sh

.PHONY: build clean

build:
	ocamlbuild -use-menhir main.native

clean:
	rm -rf _build
	rm -f main.native
