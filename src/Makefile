SHELL = /bin/sh

.PHONY: build clean

build:
	ocamlbuild -use-menhir run.native

clean:
	rm -rf _build
	rm -f run.native
