
(** Module containing the Reachability-graph data structure, 
		formerly known as G-graph.
		It deals with:
		- generations of nodes produced during the first phase of the 
			general algorithm, namely reachability analysis; 
		- bottom-up visit used to perform component selection, 
			the second phase of the general algorithm.
*)

open My_datatypes
open Datatypes_t
open Generation
open My_loops
open Facade
open Gg


(****************************************************************************************)
(*					G-graph						*)
(****************************************************************************************)


      type t = {
        universe : My_datatypes.universe_t;
        mutable targets : Gg.Node.t list;
        mutable generations : (Generation.t list);	
      }
              
			exception Different_new_nodes of string ;;
			
			
			let print_to_file file_buffer s =
				(Buffer.add_string !file_buffer (s ^ "\n"))

      let print_generations_a generations_array =
        let length = (Array.length generations_array) in
        for i = 0 to (length - 1) do
          print_endline ("\nGeneration nr. " ^ (string_of_int i) ^ ": ");
          let current_nodes_list = generations_array.(i) in 
          (Gg.Node.print_list current_nodes_list)
        done  

      let print_generations_full_a generations_array =
        let length = (Array.length generations_array) in
        for i = 0 to (length - 1) do
          print_endline ("\nGeneration nr. " ^ (string_of_int i) ^ ": ");
          let current_nodes_list = generations_array.(i) in 
          (Gg.Node.print_list_full current_nodes_list)
        done  
      
		  let get_targets graph = graph.targets

		  let get_generations graph = graph.generations
		
      (* used to update target field if/when we find_in_list it in the initial top-down phase *)
      let set_targets graph nodes =
              graph.targets <- nodes

      let set_generations graph newGen = 
              graph.generations <- newGen

      let create the_universe =
				{ 
					universe = the_universe; 
        	targets = []; 
        	generations = []
				}
 
     let clone_with_empty_gen graph =        
              {  
                 universe = graph.universe; 
                 targets = graph.targets;
                 generations = []
              }
    
    let generations_num graph =
      (List.length graph.generations) 

    let get_initial_generation graph =
      let genNum = (List.length graph.generations) in
      let initialGen = (List.nth graph.generations (genNum - 1)) in
      initialGen   
   
    let nth_generation graph i =
      (List.nth graph.generations i)      
    
    let add_generation graph gen = 
	    graph.generations <- gen :: graph.generations
		
    let print_generations graph file_buffer =
			(List.iter (Generation.print file_buffer) graph.generations) 
		
    let print_generations_full graph =
			(List.iter Generation.print_full graph.generations) 
		
		(* build first generation of the G-graph: all components in their initial state *)
    let build_initial_gen graph =
			let firstGen = Generation.create_empty in
			let buildInitialNode component =
				let initialNode = (Gg.Node.build_initial (ref component)) in
				(Generation.add_node firstGen initialNode) in
        begin
				  List.iter buildInitialNode graph.universe;
				  firstGen
        end 

    let reverse_generations graph =
      let reverse_generations = List.rev graph.generations in
      (set_generations graph reverse_generations) 
			
		let delete_targets nodes targets =
			let targets_in = (List.filter (fun x -> (Gg.Node.in_list x !targets)) nodes) in
			targets := (List.filter (fun x -> (Gg.Node.not_in_list x targets_in)) !targets);
			(* (Gg.Node.print_list targets_in); *)
			(* (Gg.Node.print_list !targets); (print_endline ""); *)
			targets_in
						
		(* generate the whole G-graph with all generations *)
    let populate graph targets =
			(* first build initial generation *)
			let firstGen = (build_initial_gen graph) in 
			(add_generation graph firstGen);
			(* initialize needed structures *)
      let newNodes = ref [] in
			(* Loop for building and adding one generation at a time *)
			(repeat_until 
				(* Loop body *)
				(fun i ->
          begin 
						i := !i + 1;
						(* create new generation from current one *)
            let currentGen = (List.hd graph.generations) in 
            let currentIndex = (Generation.get_index currentGen) in
            let newGen = (Generation.create_with_index (currentIndex+1)) in
						(* compute nodes not in previous generation *)
            newNodes := (Generation.compute_new_nodes currentGen); 
            (* build copy nodes and arcs to previous generation *)
						let copyNodes = (Gg.Node.build_copy_nodes (Generation.get_nodes currentGen)) in
						(* new generation nodes = fresh nodes + replica nodes (of nodes in current gen.) *)
            let allNodes = (copyNodes @ !newNodes) in
						(Generation.set_nodes newGen allNodes);
						(* add new generation to the G-graph *)
						(add_generation graph newGen);
						(* if we find target nodes we update targets field in the G-graph *)
						(* We also remove the targets nodes in targets ref *)
						graph.targets <- (delete_targets !newNodes targets) @ graph.targets;
						(* (print_string "nodi ");(Gg.Node.print_list !newNodes); (print_endline "");       *)
						(* (print_string "target ");(Gg.Node.print_list graph.targets); (print_endline ""); *)
						i
          end)
				(* Loop condition: stop when we reach a fixpoint (no new nodes are added) or we find target *)
				(fun i -> (Gg.Node.is_empty !newNodes) || (Gg.Node.is_empty !targets)) 
      ~init:(ref 0));
       (* alignment of generations index and index of the list containing
       * generations => need to reverse generations *)
      (reverse_generations graph) 
    
		(* this function takes care of a node that is initial: we add it as is (no
     * need to choose parent and providers) to current generation and we mark it
     * to be examined at next level (add it to prev_wset) *)
    let handle_initial_node init_node current_gen prev_wset =
      let copy_parent = (Gg.Node.clone init_node) in
      let copy_arc = Some (Gg.Copy_arc.make (ref copy_parent)) in
      (Gg.Node.set_copy init_node copy_arc);      
      (Generation.add_node current_gen init_node);
			prev_wset := (Gg.Node.add_no_duplicate copy_parent !prev_wset)
    
    (** Function used to choose providers nodes for the requires of
     		[node]. It adds arcs to current generation and providers to next working
     		set. *)
    let handle_providers ~heuristics_on file_buffer node current_gen prev_wset graph =
      (* search for providers in the previous generation *)
      let current_gen_index = (Generation.get_index current_gen) in      
      let providers_gen = (nth_generation graph (current_gen_index - 1)) in 
      let nodes = (Generation.get_nodes providers_gen) in
      (* it also adds bind arcs *)
      let ports_and_providers = (Gg.Node.choose_providers ~heuristics_on file_buffer node nodes) in
      let providers = (Gg.Node.elim_duplicates (snd (List.split ports_and_providers))) in
			IFDEF VERBOSE THEN
				(print_to_file file_buffer ("list of chosen providers: " ^ Gg.Node.to_string_list providers))
			END;
      (* all chosen providers must enter into previous working set *) 
			prev_wset := (Gg.Node.add_list_no_duplicate providers !prev_wset)

    (** This function chooses a parent node for [node] and adds it to the next 
    		working set. *)
    let handle_origin ~heuristics_on file_buffer node prev_wset =
			(* choose the right function whether relying on heuristics or not *)
			let origin = match heuristics_on with
				(* origin choice + update accordingly the fanIn field of nodes at the same level *)
				true -> (Gg.Node.choose_origin_heuristics file_buffer node)
			| false ->(Gg.Node.choose_origin file_buffer node)
			in
      (* origin is now father of node *)
      (Gg.Node.set_origin node (ref origin));
      (* node is now among the sons of origin *)
      (Gg.Node.add_son origin (ref node)); 
			IFDEF VERBOSE THEN
				(print_to_file file_buffer ("origin node chosen: " ^ (Gg.Node.to_string origin)))
			END;
      (* add parent to previous working set *) 
			prev_wset := (Gg.Node.add_no_duplicate origin !prev_wset);
			origin

    let init_gen_array graph =
      let size = (generations_num graph) in
      let generations_array = (Array.make size []) in
      generations_array

    let set_nth_generation_a gen_array generation index =
      let nodes_list = (Generation.get_nodes generation) in      
      gen_array.(index) <- nodes_list        
    
    let get_nth_nlist gen_array index =
      gen_array.(index) 
    
    let set_nth_nlist gen_array nodes_list index =
      gen_array.(index) <- nodes_list        
            
   let add_nodes_list generations_array current_gen =
    let index = (Generation.get_index current_gen) in  
    (set_nth_generation_a generations_array current_gen index)
    
    (* this function builds the initial nodes list made of only used initial nodes *)  
    let handle_initial_nlist generations_array prev_wset =
      (* for now first nodes list is made of nodes found in the last working set, i.e. prevWset, ok?  *)	
      (set_nth_nlist generations_array !prev_wset 0)

    (* Bottom-up visit for component selection *)
    let visit ~heuristics_on file_buffer graph = 
			(*print_endline ("\n ----------------- START BOTTOM-UP VISIT ------------------------- ");*)
      let generations_array = (init_gen_array graph) in
      (*let newGraph = (clone_with_empty_gen graph) in *)
      let nrlevels = (generations_num graph) in
      let last_level = (nrlevels - 1) in
			(* i-th entry in the array corresponds to working set P_i *) 
			let workSets = Array.make nrlevels [] in
		  (* at the beginning last working set, P_(nrlevels-1), contains only the target node *)
			workSets.(last_level) <- graph.targets;
      let prevWset = (ref []) in (* need to declare it here for a trick to fix initial generation *)
      (* work bottom up, stopping at the level before-the-last, i.e. 1 *)
			for l = last_level downto 1 do
			  begin	
					let workingSet = (ref workSets.(l)) in
          prevWset := workSets.(l-1);
          (* compute the fanIn field of nodes in previous generation *)
					let prevGeneration = (nth_generation graph (l-1)) in
					let prevNodes = (Generation.get_nodes prevGeneration) in
					(Gg.Node.compute_fanIn file_buffer prevNodes workingSet); 
					IFDEF VERBOSE THEN
						(Gg.Node.print_fanIn file_buffer prevNodes) 
					END;
          let currentGen = (Generation.create_with_index l) in
					(* Loop *)
					(repeat_until 
						(* Loop body *)
						(fun i ->
								i := !i + 1;
								let nodeToExamine = (Gg.Node.extract workingSet) in
								IFDEF VERBOSE THEN
									(print_to_file file_buffer ("\n ----------------- BOTTOM-UP VISIT" 
					        	^ " LEVEL nr. " ^ (string_of_int l) 
                  	^ " CYCLE execution nr. "^ (string_of_int (!i+1))^" ----------------"));
                 	(print_to_file file_buffer ("node to be examined: " ^ (Gg.Node.to_string nodeToExamine)))
								END;
                 (* if it's an initial node: simply add it to current generation and to previous working set *)
                 if (Gg.Node.is_initial nodeToExamine) then begin
										IFDEF VERBOSE THEN
		                	(print_to_file file_buffer ((Gg.Node.to_string nodeToExamine) 
												^ " is an initial node => no need to look for parent and providers"))
										END;	
                    (handle_initial_node nodeToExamine currentGen prevWset); 
                 end else begin       
                 (* else need to choose parent and (maybe) providers for fulfilling its requires *)
										IFDEF VERBOSE THEN
								    	(print_to_file file_buffer ((Gg.Node.to_string nodeToExamine) 
												^ " is NOT an initial node => need to look for parent"))
										END;	
                    (* the extracted node becomes part of the new graph *) 
                    (Generation.add_node currentGen nodeToExamine);      
                    let origin = (handle_origin ~heuristics_on file_buffer nodeToExamine prevWset) in
										(Gg.Node.update_fanIn file_buffer origin prevNodes);
                    (* if it's not a copy then choose providers *)
                    if (Gg.Node.not_a_copy nodeToExamine) then begin
											IFDEF VERBOSE THEN
								    		(print_to_file file_buffer ((Gg.Node.to_string nodeToExamine)
                      		^ " is NOT a copy => must take care of providers")); 
											END;	
                      (handle_providers ~heuristics_on file_buffer nodeToExamine currentGen prevWset graph);
                    end;  
                  end;
                  workSets.(l-1) <- !prevWset;
									IFDEF VERBOSE THEN
                  	(print_to_file file_buffer ("current generation: " 
											^ (Generation.to_string currentGen)));
										(print_to_file file_buffer ("next working set, at level nr." 
											^ (string_of_int (l-1)) ^ " : " ^ "{ " ^ (Gg.Node.to_string_list !prevWset) ^ " }"));
										(Gg.Node.print_fanIn file_buffer prevNodes) 
									END;	
									i)
							(* Loop condition: stop when there are no more unexamined nodes *)
							(fun i -> (Gg.Node.is_empty !workingSet)) 
            ~init:(ref 0));
            (*(add_generation newGraph currentGen);*)
            (add_nodes_list generations_array currentGen);
					end
        done;
        (* set the initial generation in the new graph by exploiting prevWset *)
        (handle_initial_nlist generations_array prevWset);
        generations_array
    
    (* Bottom-up visit to generate instance lines (each one corresponds to a maximal path) *)
    let linearize generations_array targets = 
			(*print_endline ("\n ----------------- START TO LINEARIZE ------------------------- ");*)
      let last_level = ((Array.length generations_array) - 1) in
      let trimmed_paths_list = (ref []) in
		  for l = last_level downto 1 do
        let current_nodes = generations_array.(l) in 
        (*print_string "current nodes: "; (Gg.Node.print_list current_nodes);*)       
        let working_set = (ref []) in  
        if (l = last_level) then
          working_set := targets
        else begin
          let next_nodes = generations_array.(l+1) in 
          (*print_string "\nnext nodes: "; (Gg.Node.print_list next_nodes);*) 
          working_set := (Gg.Node.get_finals next_nodes current_nodes) 
        end;
        (*print_string ("\nFinal nodes at level nr." ^ (string_of_int l) ^ ": ");*)
        (*(Gg.Node.print_list !working_set); print_string "\n";*)
				(* Loop condition: stop when there are no more unexamined nodes *)
        let i = (ref 0) in
        while (Gg.Node.not_empty !working_set) do
						  (*print_endline ("\n ----------------- LINEARIZATION" 
					    ^ " LEVEL nr. " ^ (string_of_int l) 
              ^ " CYCLE execution nr. "^ (string_of_int (!i+1))^"
              ----------------");*)
							i := !i + 1;
							let node_to_examine = (Gg.Node.extract working_set) in
              (*print_endline ("final node to be examined: " ^ (Gg.Node.to_string node_to_examine));*)
              (*let path = (List.rev (Gg.Node.find_full_path_to_root node_to_examine)) in*)
              (*let to_string_path = Gg.Node.to_string_list in*)
              (*print_endline ("the computed FULL path is: " ^ (to_string_path path));  *)
              let trimmed_path = (List.rev (Gg.Node.find_trim_path_to_root node_to_examine)) in
              trimmed_paths_list := trimmed_path :: !trimmed_paths_list;
              (*print_endline ("while the TRIMMED path is: " ^ (to_string_path trimmed_path));*)  
        done
      done;
      !trimmed_paths_list

