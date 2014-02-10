# OCAMLC=ocamlfind ocamlc -package js_of_ocaml -linkpkg -I ../corejs -c
OCAMLC=ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -linkpkg -I ../corejs/ -g

all:
	$(OCAMLC) -c frp.mli
	$(OCAMLC) -c frp.ml
	ocamlmklib -o frp frp.cmo

clean:
	rm frp.cmo frp.cmi
