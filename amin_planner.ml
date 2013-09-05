
open Universe_translator
open Datatypes_t
open Instance
open My_scc
open Facade
open Ggraph
open Gg
open T
open Plan

(* Variables corresponding to arguments *)

(* input / output *)
let universe_channel              = ref stdin
let output_channel                = ref stdout
let	target_component_name					= ref "" 
let	target_state 									= ref "" 

(* Arg module settings *)

let usage = 
  Printf.sprintf
    "usage: %s %s %s %s %s"
    Sys.argv.(0)
    "[-u input-universe-file]"
    "[-c target-component-type]"
    "[-s target-state]"
    "[-o output-file]"
    
let speclist = 
  Arg.align [
    (* Input arguments *)
    ("-u",         Arg.String (fun filename -> universe_channel := (open_in filename)), " The universe input file");
    ("-c",         Arg.String (fun component_name -> target_component_name := component_name), " The target component");
    ("-s",         Arg.String (fun component_state -> target_state := component_state), " The target state name");
    ("-o",         Arg.String (fun filename -> output_channel := (open_out_gen [Open_creat;Open_trunc;Open_wronly] 0o666 filename)), " The output file with the final plan")
  ] 

(* Read the arguments *)
let () =
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage


(* read universe in *)
let file_length = (in_channel_length !universe_channel) 
let buffer = (String.create file_length)
let () =
	(really_input !universe_channel buffer 0 (file_length - 1));
	(close_in !universe_channel)
let user_universe = (Datatypes_j.universe_of_string buffer) 

(* we first translate the universe provided by the user into one 
 * in which every state has an ID = (key,value) where:
 *  - key is the position in the array representation of the automaton
 *  - value is the name provided by the user
 * In the rest we work then with this representation.
 *)
let universe = (Universe_translator.translate user_universe)
(*
let () = (print_endline "performed universe translation")
*)

(* find target component in provided universe *)
(* TODO: abort if it can't find it *)
let targetType = (Facade.find_component_by_name universe !target_component_name) 

(* TODO: check for correctness: if the given state ID does not belong to the given component abort *)
(* kept for simplicity, just a matter of names *)
let targetStateID = target_state 

(*
let () =
(Printf.fprintf !output_channel "%s" "here go the results")
*)
(* setup file for output *)
(*
let out_file_name =
match in_file_name with
	("input/" ^ actual_in_file_name ^ ".json") -> 
		("results/" ^ actual_in_file_name ^ "_results.txt")
| _ -> "results/results.txt" 
in
*)

let ggraph = (Ggraph.create universe (ref targetType) !targetStateID) 
let () =
	let file_buffer = ref (Buffer.create 500) in ();
  
	(Printf.bprintf !file_buffer "%s\n" "\nWe generate the FULL G-GRAPH: \n");
	(Ggraph.populate ggraph);
	
	(Ggraph.print_generations ggraph file_buffer);
	(Printf.bprintf !file_buffer "%s\n" ("\nBOTTOM-UP VISIT of the G-graph. "
    ^ "For every node we choose origin node and providers."));
  let polished_array = (Ggraph.visit ggraph) in
  begin
    let target = (Ggraph.get_target ggraph) in
		(Printf.bprintf !file_buffer "%s\n" "\nLINEARIZATION phase");
    let trimmed_paths_list = (Ggraph.linearize polished_array target) in
    let paths_string = (Gg.Node.to_string_list_of_list trimmed_paths_list) in
		(Printf.bprintf !file_buffer "%s\n" ("\nThe linearized paths are the following:\n\n" ^ paths_string));

		let maximal_paths = (Instance.filter_maximal_paths trimmed_paths_list) in ();
			
    let instance_lines = (Instance.build_instance_lines maximal_paths) in
		(Printf.bprintf !file_buffer "%s\n" "\n\nThe INSTANCE LINES are the following:\n");
    (Instance.print_list file_buffer instance_lines);
		(Printf.bprintf !file_buffer "%s\n" "\nNext we ADD GO (blue) and RETURN (red) EDGES. ");
    (Instance.list_add_dep_edges instance_lines);
		(Printf.bprintf !file_buffer "%s\n" "\n\nNow the INSTANCE LINES WITH EDGES look like this:\n");
    (Instance.print_list file_buffer instance_lines);
		
		(*print_endline "\nNow we merge all vertices together for topological sorting.";*)
    let all_vertices = (ref (Instance.list_to_vertices instance_lines)) in ();
    (*(T.Vertex.print_list all_vertices); *)
	
		(* eliminate cycles if there are *)
		let cycles = (My_scc.find_cycles !all_vertices file_buffer) in
		if cycles != [] then
			begin
				let new_instance_lines = (My_scc.elim_cycles instance_lines cycles file_buffer) in
    		all_vertices := (Instance.list_to_vertices new_instance_lines);
				(Printf.bprintf !file_buffer "%s\n" "\n\nThe INSTANCE LINES after splitting are the following:\n");
    		(Instance.print_list file_buffer new_instance_lines)
			end;

    
		let vertices_to_sort = !all_vertices in ();
		(* proceed with the topological sorting *)
		(Printf.bprintf !file_buffer "%s\n" "\nNow we perform a TOPOLOGICAL SORT.");
    (*
		let sorted_vertices = (T.Vertex.top_sort_DEBUG vertices_to_sort) in ();
		*)
    let sorted_vertices = (T.Vertex.top_sort vertices_to_sort) in ();
		(print_endline "\nThe COMPUTED PLAN is:\n");
		(Printf.bprintf !file_buffer "%s\n" "\nAnd finally the COMPUTED PLAN is:\n");
    (T.Vertex.print_actions file_buffer sorted_vertices);

(*	
		(print_endline "\nNow we GENERATE the PLAN.");
		(Printf.bprintf !file_buffer "%s\n" "\nNow we GENERATE the PLAN.");
		let plan = (T.Vertex.synthesize_plan all_vertices) in ();
		(print_endline "\nThe COMPUTED PLAN is:\n");
		(Printf.bprintf !file_buffer "%s\n" "\nAnd finally the COMPUTED PLAN is:\n");
    (T.Vertex.print_plan file_buffer plan);
*)


		(Buffer.output_buffer !output_channel !file_buffer);
		(close_out !output_channel);
  end

