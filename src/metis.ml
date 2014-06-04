
open Json_zephyrous_output_j
open Translator
open Universe_translator
open Datatypes_t
open Instance
open Facade
open Ggraph
open Gg
open T
open Plan


exception Some_target_not_found of string


(* Variables corresponding to arguments *)

(* input / output *)
let universe_channel              = ref stdin
let conf_channel                  = ref stdin
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
		("-conf",      Arg.String (fun filename -> conf_channel := (open_in filename)), " The configuration input file");
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
(*let file_length = (in_channel_length !universe_channel) 
let buffer = (String.create file_length)
let () =
	(really_input !universe_channel buffer 0 (file_length - 1));
  (close_in !universe_channel)
	

(* let () = print_endline ("Start parsing universe file.") *)
let user_universe = (Json_zephyrous_output_j.universe_of_string buffer)*)
let user_universe = (Json_zephyrous_output_j.read_universe (Yojson.Safe.init_lexer ()) (Lexing.from_channel (!universe_channel)))

(* read configuration file *)
(*let file_length = (in_channel_length !conf_channel) 
let buffer = (String.create file_length)
let () =
	(really_input !conf_channel buffer 0 (file_length - 1));
  (close_in !conf_channel)

(* let () = print_endline ("Start parsing final configuration file.") *)
let user_conf = (Json_zephyrous_output_j.configuration_of_string buffer) *)
let user_conf = (Json_zephyrous_output_j.read_configuration (Yojson.Safe.init_lexer ()) (Lexing.from_channel (!conf_channel)))

(* convert universe into internal representation and replicate the components in the final configuration *)
let universe = Replicator.combine_universe_configurator (translate user_universe) user_conf

(* transform a list of comp_name state_name into a list of comp_type and state ref *)
let target_pairs = Replicator.get_targets user_conf
let targets = (Gg.Node.build_targets universe target_pairs)

(* build the reachability graph (previously known as G-graph) *)
let ggraph = (Ggraph.create universe) 

let () =
	let file_buffer = ref (Buffer.create 500) in (); 
	
	IFDEF VERBOSE THEN
  (Printf.bprintf !file_buffer "%s\n" "UNIVERSE");
  (Printf.bprintf !file_buffer "%s\n" (Facade.string_of_universe universe));
  END;
  
	(* populate the reachability graph with all generations by saturation *)
	(Ggraph.populate ggraph targets);
	
	
	IFDEF VERBOSE THEN
		(Printf.bprintf !file_buffer "%s\n" "\nWe generate the FULL G-GRAPH: \n"); 
		(Ggraph.print_generations ggraph file_buffer);
		(Buffer.output_buffer !output_channel !file_buffer);
		(Buffer.reset !file_buffer);

	END;
	
	if !targets != [] then
		begin
		print_endline ("Warning: one or more target can not be reached.");
		print_endline ("Not reachable targets: " ^ (Gg.Node.to_string_list !targets));
		(Buffer.output_buffer !output_channel !file_buffer);
		(Buffer.reset !file_buffer);
		exit 1
		end;
		
	IFDEF VERBOSE THEN
		(Printf.bprintf !file_buffer "%s\n" ("\nBOTTOM-UP VISIT of the G-graph. "
    	^ "For every node we choose origin node and providers."))
	END;

	(* perform component selection by bottom-up visit of the reachability graph *)	
  let polished_array = (Ggraph.visit ~heuristics_on:!use_heuristics file_buffer ggraph) in
  begin
    let targets_g = (Ggraph.get_targets ggraph) in
    let trimmed_paths_list = (Ggraph.linearize polished_array targets_g) in
		IFDEF VERBOSE THEN
			(Printf.bprintf !file_buffer "%s\n" "\nLINEARIZATION phase");
    	let paths_string = (Gg.Node.to_string_list_of_list trimmed_paths_list) in
			(Printf.bprintf !file_buffer "%s\n" ("\nThe linearized paths are the following:\n\n" ^ paths_string));
			(Buffer.output_buffer !output_channel !file_buffer);
			(Buffer.reset !file_buffer);

		END;

		(* keep only maximal paths *)
		let maximal_paths = (Instance.filter_maximal_paths trimmed_paths_list) in ();
			
		(* build single instance lines *)	
    let instance_lines = (Instance.build_instance_lines maximal_paths) in
		IFDEF VERBOSE THEN
			(Printf.bprintf !file_buffer "%s\n" "\n\nThe INSTANCE LINES are the following:\n");
    	(Instance.print_list file_buffer instance_lines);
			(Buffer.output_buffer !output_channel !file_buffer);
			(Buffer.reset !file_buffer);

		END;

		(* add dependency edges: go/blue and return/red arcs *)	
    (Instance.list_add_dep_edges instance_lines);
		IFDEF VERBOSE THEN
			(Printf.bprintf !file_buffer "%s\n" "\nNext we ADD GO (blue) and RETURN (red) EDGES.\n");
    	(Instance.print_list file_buffer instance_lines);
			(Buffer.output_buffer !output_channel !file_buffer);
			(Buffer.reset !file_buffer);

		END;

		(* deal with enclosing dependency edges, e.g. g1 g1' r1 r1' should become g1 r1' *)	
    (Instance.fix_enclosing_edges_pairs file_buffer instance_lines);
		IFDEF VERBOSE THEN
			(Printf.bprintf !file_buffer "%s" "\nNext we FIX ENCLOSING GO (blue) and RETURN (red) EDGES.");
			(Printf.bprintf !file_buffer "%s\n" "\nNow the INSTANCE LINES WITH EDGES look like this:\n");
    	(Instance.print_list file_buffer instance_lines);
			(Buffer.output_buffer !output_channel !file_buffer);
		  (Buffer.reset !file_buffer);

			
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
			(Printf.bprintf !file_buffer "\n\n%s\n" "----------------------- PLAN SYNTHESIS START -----------------------");
			(Buffer.output_buffer !output_channel !file_buffer);
			(Buffer.reset !file_buffer)
		END;
		let plan = (T.Vertex.synthesize_plan ~mandriva_mode:!mandrivian_mode all_vertices target_pairs file_buffer) in
		(Printf.bprintf !file_buffer "\n%s\n" ("The computed PLAN is: " ^ (Plan.to_string ~mandriva_mode:!mandrivian_mode plan)));
		(print_string "\nThe computed "); (Plan.print ~mandriva_mode:!mandrivian_mode plan);

		(Buffer.output_buffer !output_channel !file_buffer);
		(close_out !output_channel);

  end
