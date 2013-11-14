

all:
	ocamlbuild -no-hygiene -classic-display -use-ocamlfind metis.native

clean:
	ocamlbuild -clean
