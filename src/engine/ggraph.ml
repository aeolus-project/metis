
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
        mutable target : Gg.Node.t;
        mutable generations : (Generation.t list);	
      }
              
			exception Different_new_nodes of string ;;

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
      
		  let get_target graph = graph.target

		  let get_generations graph = graph.generations
		
      (* used to update target field if/when we find_in_list it in the initial top-down phase *)
      let set_target graph node =
              graph.target <- node
		  
      let set_generations graph newGen = 
              graph.generations <- newGen

      let create puniverse targetTypeRef targetState = 
              {  
                 universe = puniverse; 
                 target = (Gg.Node.build_target targetTypeRef targetState);
                 generations = []
              }

      let clone_with_empty_gen graph =        
              {  
                 universe = graph.universe; 
                 target = graph.target;
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
		
		(* generate the whole G-graph with all generations *)
    let populate graph =
			(* first build initial generation *)
			let firstGen = (build_initial_gen graph) in 
			(add_generation graph firstGen);
			(* initialize needed structures *)
      let newNodes = (ref []) in
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
						(* if we find target node we set target field (a reference to it) in the G-graph *)
						if (Gg.Node.in_list graph.target !newNodes) then
 							  (set_target graph (Gg.Node.find_in_list graph.target !newNodes));
						i
          end)
				(* Loop condition: stop when we reach a fixpoint (no new nodes are added) or we find target *)
				(fun i -> (Gg.Node.is_empty !newNodes) || (Gg.Node.in_list graph.target !newNodes)) 
      ~init:(ref 0));
       (* alignment of generations index and index of the list containing
       * generations => need to reverse generations *)
      (reverse_generations graph) 
		
		(* generate the G-graph DEBUG version *)
    let populate_DEBUG graph file_buffer =
			(* first build initial generation *)
			let firstGen = (build_initial_gen graph) in (add_generation graph firstGen);
			(* initialize needed structures *)
      let newNodes = (ref []) in
			(* Loop for building and adding one generation at the time *)
			(repeat_until 
				(* Loop body *)
				(fun i ->
          begin 
						print_endline ("\n ----------------- POPULATE cycle body: execution nr. " ^ 
							(string_of_int (!i+1))^"----------------");
						i := !i + 1;
						(* extract current generation *)
            let currentGen = (List.hd graph.generations) in 
						print_string "current generation: "; (Generation.print file_buffer currentGen);
            let currentIndex = (Generation.get_index currentGen) in
            let newGen = (Generation.create_with_index (currentIndex+1)) in
						(* compute nodes not in previous generation *)
            newNodes := (Generation.compute_new_nodes_DEBUG currentGen); 
						print_string "new nodes: "; Gg.Node.print_list !newNodes;
            (* build copy nodes and arcs to previous generation *)
						let copyNodes = (Gg.Node.build_copy_nodes (Generation.get_nodes currentGen)) in
						(* new generation nodes = fresh nodes + replica nodes (of nodes in current gen.) *)
            let allNodes = (copyNodes @ !newNodes) in
						(Generation.set_nodes newGen allNodes);
						print_string "\n new generation: "; (Generation.print file_buffer newGen);
						(* add new generation to the G-graph *)
						(add_generation graph newGen);
						if (Gg.Node.in_list graph.target !newNodes) then
							begin	
 							  (set_target graph (Gg.Node.find_in_list graph.target !newNodes));
							  print_endline ("target updated to " ^ (Gg.Node.to_string_full graph.target)) 
							end;
						i
          end)
				(* Loop condition: stop when we reach a fixpoint (no new nodes are added) or we find target *)
				(fun i -> (Gg.Node.is_empty !newNodes) || (Gg.Node.in_list graph.target !newNodes)) 
      ~init:(ref 0));
       (* alignment of generations index and index of the list containing
       * generations => need to reverse generations *)
      (reverse_generations graph) 
		
		(* generate the G-graph, version for checking the new nodes computation *)
    let populate_CHECK graph file_buffer =
			(* first build initial generation *)
			let firstGen = (build_initial_gen graph) in (add_generation graph firstGen);
			(* initialize needed structures *)
      let newNodes_check = (ref []) in
      let newNodes = (ref []) in
			(* Loop for building and adding one generation at the time *)
			(repeat_until 
				(* Loop body *)
				(fun i ->
          begin 
						i := !i + 1;
						(* extract current generation *)
            let currentGen = (List.hd graph.generations) in 
            let currentIndex = (Generation.get_index currentGen) in
            let newGen = (Generation.create_with_index (currentIndex+1)) in
						(* compute nodes not in previous generation *)
            newNodes_check := (Generation.compute_new_nodes currentGen); 
            newNodes := (Generation.old_compute_new_nodes currentGen);
						if (Gg.Node.list_neq !newNodes_check !newNodes) then begin  
							let msg1 = "\n\n ##################### NEW NODES DIFFER #############################" in
							let msg2 = ("\n************************** New nodes according to NEW COMPUTATION: " ^ (Gg.Node.to_string_list !newNodes_check)) in
							let msg3 = ("\n****************************************************** New nodes: " ^ (Gg.Node.to_string_list !newNodes)) in
							let error_msg = msg1 ^ msg2 ^ msg3 in
							raise (Different_new_nodes error_msg) 
						end;
            (* build copy nodes and arcs to previous generation *)
						let copyNodes = (Gg.Node.build_copy_nodes (Generation.get_nodes currentGen)) in
						(* new generation nodes = fresh nodes + replica nodes (of nodes in current gen.) *)
            let allNodes = (copyNodes @ !newNodes) in
						(Generation.set_nodes newGen allNodes);
						(* add new generation to the G-graph *)
						(add_generation graph newGen);
						if (Gg.Node.in_list graph.target !newNodes) then
 							(set_target graph (Gg.Node.find_in_list graph.target !newNodes));
						i
          end)
				(* Loop condition: stop when we reach a fixpoint (no new nodes are added) or we find target *)
				(fun i -> (Gg.Node.is_empty !newNodes) || (Gg.Node.in_list graph.target !newNodes)) 
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
    
    (* this function is used to choose providers nodes for the requires of
     * "node". It adds arcs to current generation and providers to next working
     * set. *)
    let handle_providers node current_gen prev_wset graph =
      (* search for providers in the previous generation *)
      let current_gen_index = (Generation.get_index current_gen) in      
      let providers_gen = (nth_generation graph (current_gen_index - 1)) in 
      let nodes = (Generation.get_nodes providers_gen) in
      (* it also adds bind arcs *)
      let ports_and_providers = (Gg.Node.choose_providers node nodes) in
      let providers = (Gg.Node.elim_duplicates (snd (List.split ports_and_providers))) in
      (* 
			print_endline ("list of chosen providers: " ^ Gg.Node.to_string_list providers);
			*)
      (* all chosen providers must enter into previous working set *) 
			prev_wset := (Gg.Node.add_list_no_duplicate providers !prev_wset)

    (* this function chooses a parent node for "node" and adds it the next 
     * working set *)
    let handle_origin node prev_wset =
		  let origin = (Gg.Node.choose_origin node) in
      (* origin is now father of node *)
      (Gg.Node.set_origin node (ref origin));
      (* node is now among the sons of origin *)
      (Gg.Node.add_son origin (ref node)); 
			(*
			print_endline ("origin node chosen: " ^ (Gg.Node.to_string origin));
			*)
      (* add parent to previous working set *) 
			prev_wset := (Gg.Node.add_no_duplicate origin !prev_wset)

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
    let visit graph = 
			(*print_endline ("\n ----------------- START BOTTOM-UP VISIT ------------------------- ");*)
      let generations_array = (init_gen_array graph) in
      (*let newGraph = (clone_with_empty_gen graph) in *)
      let nrlevels = (generations_num graph) in
      let last_level = (nrlevels - 1) in
			(* i-th entry in the array corresponds to working set P_i *) 
			let workSets = Array.make nrlevels [] in
		  (* at the beginning last working set, P_(nrlevels-1), contains only the target node *)
			workSets.(last_level) <- [ graph.target ];
      let prevWset = (ref []) in (* need to declare it here for a trick to fix initial generation *)
      (* work bottom up, stopping at the level before-the-last, i.e. 1 *)
			for l = last_level downto 1 do
			  begin	
					let workingSet = (ref workSets.(l)) in
          prevWset := workSets.(l-1);
          (* compute the fanIn field of nodes in previous generation *)
					(Gg.Node.compute_fanIn prevWset workingSet); 
          let currentGen = (Generation.create_with_index l) in
					(* Loop *)
					(repeat_until 
						(* Loop body *)
						(fun i ->
                (*    
								print_endline ("\n ----------------- BOTTOM-UP VISIT" 
					        ^ " LEVEL nr. " ^ (string_of_int l) 
                  ^ " CYCLE execution nr. "^ (string_of_int (!i+1))^" ----------------");
                *)  
								i := !i + 1;
								(*print_endline ("current working set, at level nr." ^
                 * (string_of_int l) ^ " : " ^ "{ " ^ (Gg.Node.to_string_list
                 * !workingSet) ^ " }");	*)
								let nodeToExamine = (Gg.Node.extract workingSet) in
                 (*print_endline ("node to be examined: " ^ (Gg.Node.to_string nodeToExamine));*)
                 (* if it's an initial node: simply add it to current generation and to previous working set *)
                 if (Gg.Node.is_initial nodeToExamine) then begin
		                (*print_endline ((Gg.Node.to_string nodeToExamine) ^ " is an initial node => no need to look for parent and providers");*)	
                    (handle_initial_node nodeToExamine currentGen prevWset); 
                 end else begin       
                 (* else need to find_in_list parent and (maybe) providers for fulfilling the requires *)
								    (*print_endline ((Gg.Node.to_string nodeToExamine) ^ " is NOT an initial node => need to look for parent");	*)
                    (* the extracted node becomes part of the new graph *) 
                    (Generation.add_node currentGen nodeToExamine);      
                    (handle_origin nodeToExamine prevWset);
										(*print_endline ("next working set after adding ORIGIN: " ^ "{ " ^ (Gg.Node.to_string_list !prevWset) ^ " }");	*)
                    (* if is not a copy then choose provider *)
                    if (Gg.Node.not_a_copy nodeToExamine) then begin
								     (* print_endline ((Gg.Node.to_string nodeToExamine)
                      ^ " is NOT a copy => must take care of providers");*) 
                      (handle_providers nodeToExamine currentGen prevWset graph)
                    end;  
                  end;
                  workSets.(l-1) <- !prevWset;
                  (*
                  print_endline ("current generation: " ^  (Generation.to_string currentGen));
									print_endline ("next working set, at level nr." ^ (string_of_int (l-1)) ^ " : " ^ "{ " ^ (Gg.Node.to_string_list !prevWset) ^ " }");
                  *)  
									i)
							(* Loop condition: stop when there are no more unexamined nodes *)
							(fun i -> (Gg.Node.is_empty !workingSet)) 
            ~init:(ref 0));
            (*(add_generation newGraph currentGen);*)
            (add_nodes_list generations_array currentGen);
					end
        done;
        (* set the initial generation in the new graph by exploiting prevWset *)
        (*(handle_initial_gen newGraph prevWset);*)
        (handle_initial_nlist generations_array prevWset);
        generations_array
    
    (* Bottom-up visit to generate instance lines (each one corresponds to a maximal path) *)
    let linearize generations_array target = 
			(*print_endline ("\n ----------------- START TO LINEARIZE ------------------------- ");*)
      let last_level = ((Array.length generations_array) - 1) in
      let trimmed_paths_list = (ref []) in
		  for l = last_level downto 1 do
        let current_nodes = generations_array.(l) in 
        (*print_string "current nodes: "; (Gg.Node.print_list current_nodes);*)       
        let working_set = (ref []) in  
        if (l = last_level) then
          working_set := [ target ]
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

