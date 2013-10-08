

all:
	ocamlbuild -classic-display -use-ocamlfind amin_planner.native
	#ocamlbuild -classic-display -use-ocamlfind check_new_nodes_computation.native
	#ocamlbuild -classic-display -use-ocamlfind -libs batteries amin_planner.native

debug:
	ocamlbuild -classic-display -use-ocamlfind amin_planner_DEBUG.native
	
clean:
	rm -f *.cmi
	rm -f *.cmx
	rm -f *.cmo
	rm -f *.o
