OCAMLC=ocamlc -I ../corejs -c

all:
	$(OCAMLC) frp.mli frp.ml

clean:
	rm frp.cmo frp.cmi
