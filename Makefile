OCAMLC=ocamlfind ocamlc -package js_of_ocaml -I ../corejs -c

all:
	$(OCAMLC) frp.mli frp.ml

clean:
	rm frp.cmo frp.cmi
