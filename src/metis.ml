
open Universe_translator
open Datatypes_t
open Instance
open Facade
open Ggraph
open Gg
open T
open Plan


(* Variables corresponding to arguments *)

(* input / output *)
let universe_channel              = ref stdin
(* sequential plan output channel *)
let output_channel                = ref stdout
(* (a)bstract (p)lan output channel *)
let ap_output_channel             = ref stdout
let	target_component_name					= ref "" 
let	target_state 									= ref "" 

(* settings for Metis' behaviour *)
let use_heuristics								= ref false
let mandrivian_mode								= ref false

(* Arg module settings *)

let usage = 
  Printf.sprintf
    "usage: %s %s %s %s %s %s %s %s"
    Sys.argv.(0)
    "[-hr]"
    "[-m]"
    "[-u input-universe-file]"
    "[-c target-component-type]"
    "[-s target-state]"
    "[-o output-file]"
    "[-ap output-file-abstract-plan]"
    
let speclist = 
  Arg.align [
    (* Input arguments *)
		("-hr",        Arg.Set use_heuristics, " Use heuristics for component selection");
		("-m",         Arg.Set mandrivian_mode, " Work in Mandriva mode");
    ("-u",         Arg.String (fun filename -> universe_channel := (open_in filename)), " The universe input file");
    ("-c",         Arg.String (fun component_name -> target_component_name := component_name), " The target component");
    ("-s",         Arg.String (fun component_state -> target_state := component_state), " The target state name");
    ("-o",         Arg.String (fun filename -> output_channel := (open_out_gen [Open_creat;Open_trunc;Open_wronly] 0o666 filename)), " The output file with the sequential plan");
    ("-ap",        Arg.String (fun filename -> ap_output_channel := (open_out_gen [Open_creat;Open_trunc;Open_wronly] 0o666 filename)), " The output file for the abstract plan")
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

(* find target component in provided universe *)
let target_type = (Facade.find_component_by_name universe !target_component_name) 

(* build the reachability graph (previously known as G-graph) *)
let ggraph = (Ggraph.create universe (ref target_type) !target_state) 
let () =
	let file_buffer = ref (Buffer.create 500) in ();
  
	(* populate the reachability graph with all generations by sauration *)
	(Ggraph.populate ggraph);
	
	IFDEF VERBOSE THEN
		(Printf.bprintf !file_buffer "%s\n" "\nWe generate the FULL G-GRAPH: \n"); 
		(Ggraph.print_generations ggraph file_buffer);
		(Printf.bprintf !file_buffer "%s\n" ("\nBOTTOM-UP VISIT of the G-graph. "
    	^ "For every node we choose origin node and providers."))
	END;

	(* perform component selection by bottom-up visit of the reachability graph *)	
  let polished_array = (Ggraph.visit ~heuristics_on:!use_heuristics file_buffer ggraph) in
  begin
    let target = (Ggraph.get_target ggraph) in
    let trimmed_paths_list = (Ggraph.linearize polished_array target) in
		IFDEF VERBOSE THEN
			(Printf.bprintf !file_buffer "%s\n" "\nLINEARIZATION phase");
    	let paths_string = (Gg.Node.to_string_list_of_list trimmed_paths_list) in
			(Printf.bprintf !file_buffer "%s\n" ("\nThe linearized paths are the following:\n\n" ^ paths_string))
		END;

		(* keep only maximal paths *)
		let maximal_paths = (Instance.filter_maximal_paths trimmed_paths_list) in ();
			
		(* build single instance lines *)	
    let instance_lines = (Instance.build_instance_lines maximal_paths) in
		IFDEF VERBOSE THEN
			(Printf.bprintf !file_buffer "%s\n" "\n\nThe INSTANCE LINES are the following:\n");
    	(Instance.print_list file_buffer instance_lines)
		END;

		(* add dependency edges: go/blue and return/red arcs *)	
    (Instance.list_add_dep_edges instance_lines);
		IFDEF VERBOSE THEN
			(Printf.bprintf !file_buffer "%s\n" "\nNext we ADD GO (blue) and RETURN (red) EDGES.\n");
    	(Instance.print_list file_buffer instance_lines)
		END;

		(* deal with enclosing dependency edges, e.g. g1 g1' r1 r1' should become g1 r1' *)	
    (Instance.fix_enclosing_edges_pairs file_buffer instance_lines);
		IFDEF VERBOSE THEN
			(Printf.bprintf !file_buffer "%s" "\nNext we FIX ENCLOSING GO (blue) and RETURN (red) EDGES.");
			(Printf.bprintf !file_buffer "%s\n" "\nNow the INSTANCE LINES WITH EDGES look like this:\n");
    	(Instance.print_list file_buffer instance_lines)
		END;

		(* output abstract plan in DOT file *)
		if !ap_output_channel != stdout then begin
			let ap_file_buffer = ref (Buffer.create 500) in
    	(Instance.print_abstract_plan ap_file_buffer instance_lines);
			(Buffer.output_buffer !ap_output_channel !ap_file_buffer);
			(close_out !ap_output_channel)
		end;

		(* merge all vertices together before performing the adaptive topological sort *)
    let all_vertices = (ref (Instance.list_to_vertices instance_lines)) in ();
		
		(* sequential plan synthesis by means of adaptive topological sort *)
		IFDEF VERBOSE THEN
			(Printf.bprintf !file_buffer "\n\n%s\n" "----------------------- PLAN SYNTHESIS START -----------------------")
		END;
		let plan = (T.Vertex.synthesize_plan ~mandriva_mode:!mandrivian_mode all_vertices !target_component_name !target_state file_buffer) in
		(Printf.bprintf !file_buffer "\n%s\n" ("The computed PLAN is: " ^ (Plan.to_string ~mandriva_mode:!mandrivian_mode plan)));
		(print_string "\nThe computed "); (Plan.print ~mandriva_mode:!mandrivian_mode plan); 

		(Buffer.output_buffer !output_channel !file_buffer);
		(close_out !output_channel);

  end
