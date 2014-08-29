all:
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -build

clean:
	rm -r _build

install:
	./remove_existing.sh
	ocaml setup.ml -install

