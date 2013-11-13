

all:
	ocamlbuild -classic-display -use-ocamlfind metis.native

clean:
	ocamlbuild -clean
