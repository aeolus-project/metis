

all:
	ocamlbuild -classic-display -use-ocamlfind amin_planner.native

debug:
	ocamlbuild -classic-display -use-ocamlfind amin_planner_DEBUG.native
	
clean:
	rm -f *.cmi
	rm -f *.cmx
	rm -f *.cmo
	rm -f *.o
