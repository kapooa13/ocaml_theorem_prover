
all: lexer parser js
	dune build

build:
	dune build

lexer: ./lib/lexer.mll
	ocamllex  ./lib/lexer.mll

parser: ./lib/parser.mly
	ocamlyacc  ./lib/parser.mly

js: lexer parser
	dune build ./main.bc
	js_of_ocaml +base/runtime.js _build/default/main.bc

clean:
	rm -f ./lib/lexer.ml ./lib/parser.ml ./lib/parser.mli

test:
	dune runtest
