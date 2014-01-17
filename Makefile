
OBFLAGS := -no-hygiene -classic-display -use-ocamlfind 
PPFLAGS := -pp camlp4o pa_macro.cmo -DFOO

all:
	ocamlbuild $(OBFLAGS) metis.native
	#ocamlbuild $(PPFLAGS) $(OBFLAGS) metis.native

clean:
	ocamlbuild -clean
