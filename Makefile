
all: lexer parser
	dune build
	dune exec ./main.exe

build:
	dune build

lexer: ./lib/lexer.mll
	ocamllex  ./lib/lexer.mll

parser: ./lib/parser.mly
	ocamlyacc  ./lib/parser.mly

clean:
	rm -f ./lib/lexer.ml ./lib/parser.ml ./lib/parser.mli

test:
	dune runtest
