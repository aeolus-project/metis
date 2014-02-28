
OBFLAGS := -no-hygiene -classic-display -use-ocamlfind
ADDPPFLAGS := -pp 'camlp4o pa_macro.cmo -DVERBOSE'
RMPPFLAGS := -pp 'camlp4o pa_macro.cmo -UVERBOSE'

all:
	#ocamlbuild $(OBFLAGS) metis.native
	ocamlbuild $(RMPPFLAGS) $(OBFLAGS) metis.native

verbose:
	ocamlbuild $(ADDPPFLAGS) $(OBFLAGS) metis.native

doc:
	ocamlbuild $(RMPPFLAGS) $(OBFLAGS) metis.docdir/index.html

clean:
	ocamlbuild -clean
