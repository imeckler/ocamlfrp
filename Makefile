all:
	ocamlc -c frp.mli frp.ml

clean:
	rm frp.cmo frp.cmi
