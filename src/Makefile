SHELL = /bin/sh

.PHONY: build clean doc default

default: build

../deps/sexpr-wasm-prototype: ../deps
	cd ../deps; \
	git clone https://github.com/WebAssembly/sexpr-wasm-prototype.git; \
	cd sexpr-wasm-prototype; \
	git submodule update --init

../deps/sexpr-wasm-prototype/out/sexpr-wasm: ../deps/sexpr-wasm-prototype
	cd ../deps/sexpr-wasm-prototype; \
	git pull; \
	make

../deps:
	mkdir ../deps; \

../deps/binaryen:
	cd ../deps;\
	git clone https://github.com/astlouisf/binaryen.git;

#../deps
#	cd ../deps/binaryen;\
#	git pull; \
#	cmake . && make

dependencies: ../deps ../deps/sexpr-wasm-prototype/out/sexpr-wasm

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
