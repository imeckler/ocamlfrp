all:
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -build

install:
	ocaml setup.ml -install

