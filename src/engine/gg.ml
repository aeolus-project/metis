(** This module defines the the G-graph node record and arcs. 
		It contains five submodules (put together as they are mutually recursive):  
     - Node
     - Pred_arc 
     - Copy_arc 
     - Req_arc 
     - Bind_arc 
*)

open My_datatypes
open Datatypes_t
open Facade

module Gg =
  struct
    (** A node is basically a component type in a given state. *)
    module rec Node : sig
      (** Node type, to be left abstract here. *)      
      type t
      
			val get_state : t -> (state_t ref)
      val get_res_type : t -> (component_t ref)
      val get_bound_ports_nodes : t -> ((port_name * (t ref)) list)
      
			(** Set copy field. *)
      val set_copy : t -> Copy_arc.t option -> unit
      
			(** Set origin field. *)
      val set_origin : t -> (t ref) -> unit
			
			(** Add a node as a possible son of the current one. *)
      val add_son : t -> (t ref) -> unit

      (* Create a new node with the same fields as the original one. *)
      val clone : t -> t

      val to_string : t -> string
      val to_string_with_bindings : t -> string
      val to_string_full : t -> string
      val to_string_list : t list -> string
      val to_string_list_of_list : (t list) list -> string
      val to_string_list_full : t list -> string
      val print_list : t list -> unit
      val print_list_full : t list -> unit
      
			val build_succs_list : port_name list -> t list -> t list
      
			(* it creates an initial node  < T, q0 > *)
      val build_initial : (component_t ref) -> t
      
			(** Create multiple target nodes. *)
      val build_targets : (component_t ref * state_t ref) list -> t list
			(*
      val build_multiple_targets : ((component_t ref) * state_name) list -> t list
			*)      
      
			(* it creates a new node with the given pair < T, q > *)
      val build_from_pair : (component_t ref) -> state_id_t -> t
      
			(* function that retrieves the ports provided by a given list of nodes *)
      val provides_of_node_list : t list -> port_name list
      
			(* Check equality of two list of nodes. N.B. it only looks at the resource type state pair <T,q>. *)	
      val list_neq : t list -> t list -> bool
			
			(* just checks if a node is in the given list. N.B. it only looks at the resource type state pair <T,q> *)	
      val in_list : t -> t list -> bool
      
			(* just checks if a node is not in the given list. N.B. it only looks at the resource type state pair <T,q> *)	
      val not_in_list : t -> t list -> bool
      
			(* it returns the first occurrence of the given "node" in the node list "nlist" N.B. it only looks at the resource type state pair <T,q> *)	
      val find_in_list : t -> t list -> t
      
			(* dummy function to test for emptiness *)
      val is_empty : t list -> bool
      
			(* dummy function to test for emptiness *)
      val not_empty : t list -> bool
      
			(* this function takes a list and returns the list without duplicates *)
      val elim_duplicates : t list -> t list
      
			(* this function takes a list and returns the list without duplicates *)
      val elim_duplicates_simple : t list -> t list
      
			(* this function corresponds to set addition (no duplicates) of "node" to a given list *)
      val add_no_duplicate :  t -> (t list) -> (t list)
      
			(* this function simply builds a new replica R of the given node list "nlist" 
      where each node is set as a copy of the corresponding one in "nlist" *)
      val build_copy_nodes : t list -> t list 
      
			(* this function extracts the head of a list modifying the given list to contain only the tail *)	
      val extract : ((t list) ref) -> t
      (* test to see if this node is in an initial state *)  
      
			val is_initial : t -> bool      
      (* test to see if this node is the copy of someone else *)  
      
			val not_a_copy : t -> bool      
      
 			(** Choose origin node by simply picking the first available one: 
					cheaper than relying on heuristics. *)
			val choose_origin: Buffer.t ref -> t -> t
 			
			(** Choose origin node relying on heuristics. *)
			val choose_origin_heuristics: Buffer.t ref -> t -> t
      
			(* from a given node n compute a list of nodes satsfying requirements of n *)
      val choose_providers : heuristics_on:bool -> Buffer.t ref -> t -> (t list) -> ((port_name * t) list) 
			
			(* this function corresponds to set addition (no duplicates) of node list to a given list *)
      val add_list_no_duplicate : (t list) -> (t list) -> (t list)
      
			(* given a list of nodes it returns the list of nodes that are final *)  
      val get_finals : t list -> t list -> t list
      
			(* compute the path to the origin following recursively the "origin" field *)
      val find_full_path_to_root : t -> t list
      
			(* compute the path to the origin following recursively the "origin" field *)
      val find_trim_path_to_root : t -> t list
			
			val filter_by_comp_type : (component_t ref) -> (t list) list -> (t list) list  
      
			(* find a node that matches the give pair <T,q> in a list of lists of nodes *)
      val find_by_comp_state : (component_t ref) -> state_id_t -> (t list) list -> t
      
			(* used to initialize instance line ID, something like "a : A" *)
      val make_id : t list -> string
      
			(** Deal with same node reached by different paths: compactified into a single representative with all possible predecessors. *)
			val unify_successors : t list -> t list
      
			val extract_comp_type : t list -> component_t ref

			(** Computes the fanIn value of a list of nodes and sets the corresponding field. *)
			val compute_fanIn : Buffer.t ref -> t list -> (t list) ref -> unit  
			
			val print_fanIn : Buffer.t ref -> t list -> unit  
			
			(** Function used to update the fanIn value of peer nodes (nodes in the same 
					generation) once a node is chosen as origin or as provider *)		
			val update_fanIn : Buffer.t ref -> t -> t list -> unit
    
		end = struct
    (** A node of the G-graph  is made of:
	    - res_type, its component type 
	    - state, the current state
	    - preds, a list of predecessor arcs
	    - copy, an (optional) reference to the copy node 
	    - require_arcs, a list of require arcs
	    - bindings, a list of binding arcs
			- origin, an origin node (the chosen father)
			- sons, a list of the node's sons
			- boun_to_me, a list of the nodes bound to the current one
	    - card, cardinality, i.e. the nr. of requirements (computed in construction phase)
	    - dist, distance, i.e. the nr. of state-change actions (computed in construction phase)
      - fanIn, the nr. of incoming arcs (synthesized during bottom-up traversal)
			- copy_index, level of copy (n_3 is the third copy of node n)
			- is_final, tag used to identify the maximal paths
    *)
              type t = { 
                res_type : component_t ref; 
                state : state_t ref;
                mutable preds : Pred_arc.t list; 
                mutable copy : Copy_arc.t option;
                mutable require_arcs : Req_arc.t list; 
                mutable bindings : Bind_arc.t list; 
                mutable origin : (t ref) option;
                mutable sons : (t ref) list;
                mutable bound_to_me : (port_name * (t ref)) list;
                mutable card : int; 
                mutable dist : int;
                mutable fanIn : int;
                mutable copy_index : int;
                mutable is_final : bool; 
              }
              
              exception No_available_origin_node of string ;;
              exception No_available_copy_node of string ;;
              exception No_available_provider of string ;;
              exception Impossible_to_extract_node of string ;;
              exception Empty_copy_arc of string ;;
              exception No_vertex_value ;;
              exception State_not_found of string ;;
              exception No_available_origins of string ;;
              exception No_available_providers of string ;;
              exception Empty_list_of_nodes ;;
              exception No_selected_nodes ;;
            
            (* TODO: where to place these dummy functions? *)  
            (* Test for list emptiness *)
            let is_empty nlist = (nlist = [])
            (* Test for list emptiness *)
            let not_empty nlist = not (is_empty nlist)
 
 
  (**************************************************************)
  (*                      Boilerplate code  		                *)
  (**************************************************************)

  let get_state node =
    node.state

  let get_bound_ports_nodes node =
    let bound_ports_nodes = (List.map Bind_arc.get_dest node.bindings) in
    bound_ports_nodes

  let get_res_type node =
    node.res_type 

  let set_origin node nref =
    node.origin <- (Some nref)    
  
  let set_copy node copyArc =
    node.copy <- copyArc
  
  (* adds require arc to a "node" *)
  let add_require_arc node req_arc =
    node.require_arcs <- req_arc :: node.require_arcs          
  
  (* adds bind arc to a "node" *)
  let add_bind_arc node bind_arc =
    node.bindings <- bind_arc :: node.bindings          


  (**************************************************************)
  (*    Utility functions for string conversion and printing		*)
  (**************************************************************)

	let print_to_file file_buffer s =
		(Buffer.add_string !file_buffer (s ^ "\n"))
  
  let to_string node = 
    let copy_index_string = if (node.copy_index = 0) then 
                              "" 
                            else 
                              ("_" ^ (string_of_int node.copy_index)) in
	  "<" ^ !(node.res_type).cname ^ "," 
    ^ (to_string_state !(node.state)) 
    ^ ">" ^ copy_index_string 
  
  let to_string_nref nodeRef = 
	  let node = !nodeRef in
	  "< " ^ !(node.res_type).cname ^ "," ^ (to_string_state !(node.state)) ^ " >"

  let print node = print_string (to_string node)

  let print_list nlist =
	  List.iter print nlist 
  
  let preds_to_string node =
    let pred_to_string predarc =
       (to_string node) ^ (Pred_arc.to_string predarc) in     
    let stringlist = (List.map pred_to_string node.preds) in
    (String.concat "  " stringlist)
      
  let copy_to_string node =
    match node.copy with
      None -> ""
    | (Some copyarc) -> (to_string node) ^ (Copy_arc.to_string copyarc)

  let bindings_to_string node =
    let bind_string_list = (List.map Bind_arc.to_string node.bindings) in
    (String.concat ", " bind_string_list)    
 
	let to_string_with_bindings node =
		let node_str = (to_string node) in
		let bindings_str = (bindings_to_string node) in
		let string_repr = node_str ^ " has BINDINGS: " ^ bindings_str in
		string_repr
 
  let rec to_string_list nlist =
	  match nlist with 
		  [] -> ""
	  |	head :: tail -> (to_string head) ^ " " ^ (to_string_list tail)
 
  let to_string_list_of_list list_of_lists =
    let string_list = (List.map to_string_list list_of_lists) in
    (String.concat "\n\n" string_list)

  let rec to_string_ref_list ref_list =
	  match ref_list with 
		  [] -> ""
	  |	head :: tail -> (to_string !head) ^ " " ^ (to_string_ref_list tail)

  let to_string_full node =
	  let nodeString = (to_string node) in 
	  let predsString = (preds_to_string node) in 
    let sonsString = (to_string_ref_list node.sons) in
    let copyString = (copy_to_string node) in
    let bindingsString = (bindings_to_string node) in
    nodeString
	  ^ " with dist = " ^ (string_of_int node.dist)
	  ^ " and card = " ^ (string_of_int node.card)
	  ^ " and preds = { " ^ predsString ^ " }"
	  ^ " and sons = { " ^ sonsString ^ " }"
	  ^ " and copy = { " ^ copyString ^ " }" 
	  ^ " and bindings = { " ^ bindingsString ^ " }\n" 
  
  let rec to_string_list_full nlist =
	  match nlist with 
		  [] -> ""
	  |	head :: tail -> (to_string_full head) ^ " " ^ (to_string_list_full tail)
  
  let print_full node = print_string (to_string_full node)

  let print_list_full nlist =
	  List.iter print_full nlist 
 
  
	(****************************************************)
  (*				    utility functions on nodes    				*)
  (****************************************************)
 
  (* this function extracts the origin node from the origin field *)  
  let get_origin_node node =
    match node.origin with
      (Some node_ref) -> !node_ref
    | None -> raise (No_available_origin_node ("node " ^ (to_string node) ^ " has no origin!" ))
  
	(* this function extracts the copy node from the copy field *)  
  let get_copy_node node =
    match node.copy with
      (Some copy_arc) -> !(Copy_arc.get_dest copy_arc)
    | None -> raise (No_available_copy_node ("node " ^ (to_string node) ^ " has no copy!" ))

  (* TODO: comment *) 
  let add_son node nref =
    node.sons <- nref :: node.sons      
      
  (* TODO: comment *) 
  let add_node_bound_to_me node port nref =
    node.bound_to_me <- (port, nref) :: node.bound_to_me

  (** Test to see if this node is in an initial state. *)  
  let is_initial node =       
    (is_initial_state !(node.state))
  
	(** Test to see if this node is not a copy of someone else. *)  
  let is_a_copy node =
   match node.copy with
      None -> false
   |  (Some nref) -> true
  
  (** Test to see if this node is not a copy of someone else. *)  
  let not_a_copy node =
   match node.copy with
      None -> true
   |  (Some nref) -> false
              
  (** Check node equality by looking only at resource type state pair. *)
  let pair_eq n1 n2 =
	  (!(n1.res_type).cname = !(n2.res_type).cname) && (n1.state = n2.state) 
  
  (** Check node in-equality by looking only at resource type state pair. *)
  let pair_not_eq n1 n2 =
	  (!(n1.res_type).cname != !(n2.res_type).cname) || (n1.state != n2.state) 

	
	let rec list_neq_aux nlist1 nlist2 =
		match nlist1 with
			[] -> false
		| head :: tail ->
			((not (pair_eq head (List.hd nlist2))) 
				|| (list_neq_aux tail (List.tl nlist2))) 

	(** Check equality of two list of nodes *) 
	let list_neq nlist1 nlist2 =
		let length1 = (List.length nlist1) in 
		let length2 = (List.length nlist2) in 
		if length1 != length2 then 
			true
		else
			(list_neq_aux nlist1 nlist2)

  (** Check if a node is in the given list. N.B. it only looks at the resource 
      type state pair < T, q > 
  *)	
  let rec in_list node nlist =
	  match nlist with
		  [] -> false
	  |	head :: tail -> (pair_eq node head) || (in_list node tail) 
  
  (** Check if a node is not in the given list. N.B. it only looks at the 
      resource type state pair < T, q > 
  *)	
  let not_in_list node nlist = not(in_list node nlist) 
  
  (** Return the first occurrence of the given "node" in the node list "nlist". 
      N.B. it only looks at the resource type state pair <T,q>. 
  *)	
  let find_in_list node nlist = List.find (pair_eq node) nlist

	let find_all_in_list nlist1 nlist2 =
		(List.filter 

  (** Check equality of pair <T,q> of a given node and provided T' and q'. *)
  let eq_node_pair comp_type state_id node =
    let n_ctype = node.res_type in
    let n_ctype_name = (!n_ctype).cname in    
    let comp_type_name = (!comp_type).cname in    
    let n_state = !(node.state) in
    ((n_ctype_name = comp_type_name) && (n_state.id = state_id)) 
 
  (** Filter nodes of a list that match a given component type. *)
	let filter_by_comp_type comp_type list_of_nlist =
    let eq_comp comp_t nlist =
			let head_node = (List.hd nlist) in
			let nlist_comp_type = head_node.res_type in
      let nlist_comp_name = (!nlist_comp_type).cname in
      let comp_type_name = (!comp_type).cname in
      (comp_type_name = nlist_comp_name)
    in      
    let filtered_list = (List.filter (eq_comp comp_type) list_of_nlist) in
    filtered_list

  (** Find a node that matches the give pair <T,q> in a list of lists of nodes. *)
  let rec find_by_comp_state comp_type state_id list_of_nlist =
    let find_in_single_list ctype stateid nlist =
      (List.find (eq_node_pair ctype stateid) nlist) in
	  match list_of_nlist with
      [] -> raise Not_found;
	  |	head :: tail ->
        begin
          try
            let node = (find_in_single_list comp_type state_id head) in
            node;
          (* if I don't find it I go on with the search on the next node lists *)            
          with 
            Not_found -> (find_by_comp_state comp_type state_id tail) 
        end 
     
  (** This function takes a reference to a list of nodes and returns the list 
      with no occurrence of the given "node". 
  *)
  let rec delete_all_occurrences node nlist =
	  match nlist with
		  [] -> []
	  |	head :: tail -> if (pair_eq node head) then 
				              (delete_all_occurrences node tail) else 
				              head :: (delete_all_occurrences node tail)

  (** This function corresponds to set addition (no duplicates) of "node" to a 
      given list. 
  *)
  let add_no_duplicate node nlist =
    node :: (delete_all_occurrences node nlist)
  
  (** This function takes a reference to a list and returns the list without 
      duplicates. 
  *)
  let rec elim_duplicates nlist =
	  match nlist with
		  [] -> []
	  |	head :: tail -> (add_no_duplicate head (elim_duplicates tail)) 

  (** This function takes a reference to a list and returns the list without 
      duplicates. 
  *)
	let rec elim_duplicates_simple nodes =
				match nodes with
					[] -> []
				| head :: tail -> 
						begin
							if (in_list head tail) then
								(elim_duplicates_simple tail)
							else 
								head :: (elim_duplicates_simple tail)
						end
  
  (** This function corresponds to set addition (no duplicates) of nodes in 
      "nlist" to a given list. 
  *)
  let add_list_no_duplicate nlist1 nlist2 =
    let mergedList = nlist1 @ nlist2 in
    (elim_duplicates mergedList)

	(** Check if port "req" is among the list of ports "provides". *)
  let rec is_fulfilled req provides =
    (List.mem req provides)      

  (** Check if ports "requires" are all among the list of ports "provides". *)
  let rec are_fulfilled requires provides =
	  match requires with 
		  [] -> true
	  |	head :: tail -> (is_fulfilled head provides) && (are_fulfilled tail provides)

  (** Check if at least a port among "requires" is not among the list of ports
      "provides". 
  *)
  let are_not_fulfilled requires provides = 
	  not (are_fulfilled requires provides) 
  
  (** Retrieve the ports provided by a given node. *)
	let provides_of_node node =
  	let actual_state = !(node.state) in
  	actual_state.provides  

  (** Retrieve the ports provided by a given node list. *)
	let provides_of_node_list nlist =
		List.concat (List.map provides_of_node nlist)

	(** Retrieve the ports required by a given node. *)
	let requires_of_node node =
  	let actual_state = !(node.state) in
  	actual_state.requires

	(** Retrieve the ports required by a given list of nodes. *)
	let requires_of_node_list nlist =
		List.concat (List.map requires_of_node nlist)

	(** Filter the nodes from node list [nlist] that satisfy the given [require]. *)
	let filter_port_providers require nlist =
		(List.filter (fun node -> (is_fulfilled require (provides_of_node node))) nlist)
	
	let update_fanIn_single file_buffer provides node =
		let aux node provide =
			let node_provides = (provides_of_node node) in
			if (List.mem provide node_provides) then 
				if node.fanIn > 0 then begin
					node.fanIn <- node.fanIn - 1;
					IFDEF VERBOSE THEN
						(print_to_file file_buffer ("Update fanIn : fanIn[" ^ (to_string node) 
							^ "] <- " ^ (string_of_int node.fanIn) ^ " (due to provide " ^ provide ^ ")"))
					END;
				end;
		in (List.iter (aux node) provides)

	(** Function used to update the fanIn value of peer nodes (nodes in the same 
			generation) once a node is chosen as origin or as provider *)		
	let update_fanIn file_buffer node nodes =
		let provides = (provides_of_node node) in
		(List.iter (update_fanIn_single file_buffer provides) nodes)  	

	(** Find the nodes with maximum fanIn value. *)
	let find_max_fanIn_nodes nodes =
		let nodes_array = (Array.of_list nodes) in
		let compare_fanIn n1 n2 = (compare n1.fanIn n2.fanIn) in
		(* sorted in increasing order *)
		(Array.fast_sort compare_fanIn nodes_array);
		let size = (Array.length nodes_array) in
		let node_max_fanIn = nodes_array.(size-1) in
		(* keep a list of nodes with same/max fanIn value *)
		let max_fanIn_value = node_max_fanIn.fanIn in
		let has_eq_value value node = (node.fanIn == value) in 
		let max_fanIn_nodes = (List.filter (has_eq_value max_fanIn_value) nodes) in 
		max_fanIn_nodes
	
	(** Find the nodes with minimum cardinality value. *)
	let find_min_card_nodes nodes =
		let nodes_array = (Array.of_list nodes) in
		let compare_card n1 n2 = (compare n1.card n2.card) in
		(* sorted in increasing order *)
		(Array.fast_sort compare_card nodes_array);
		let node_min_card = nodes_array.(0) in
		(* keep a list of nodes with same/min card value *)
		let min_card_value = node_min_card.card in
		let has_eq_value value node = (node.card == value) in 
		let min_card_nodes = (List.filter (has_eq_value min_card_value) nodes) in 
		min_card_nodes
	
	(** Find the nodes with minimum distance value. *)
	let find_min_dist_nodes nodes =
		let nodes_array = (Array.of_list nodes) in
		let compare_dist n1 n2 = (compare n1.dist n2.dist) in
		(* sorted in increasing order *)
		(Array.fast_sort compare_dist nodes_array);
		let node_min_dist = nodes_array.(0) in
		(* keep a list of nodes with same/min dist value *)
		let min_dist_value = node_min_dist.dist in
		let has_eq_value value node = (node.dist == value) in 
		let min_dist_nodes = (List.filter (has_eq_value min_dist_value) nodes) in 
		min_dist_nodes
   

 	(** Choose origin node by simply picking the first available one. *)
	let choose_origin file_buffer node =
		let origin = match node.copy with
      (Some copy_arc) -> !(Copy_arc.get_dest copy_arc)
    | None -> if (not_empty node.preds) then
                let pred_arc = (List.hd node.preds) in
                !(Pred_arc.get_dest pred_arc)
              else
                raise (No_available_origin_node ("node " ^ (to_string node) ^ " has no origin!" ))
 		in
		IFDEF VERBOSE THEN
			(print_to_file file_buffer ((to_string origin) ^ " chosen as origin of " ^ (to_string node))) 
		END;
		origin
	
 	(** Choose origin node relying on heuristics. *)
  let choose_origin_heuristics file_buffer node =
		let get_pred pred_arc = !(Pred_arc.get_dest pred_arc) in
		let predecessors = (List.map get_pred node.preds) in
		let copy_list = match node.copy with
    		(Some copy_arc) -> [!(Copy_arc.get_dest copy_arc)]
    	| None -> [] in
		let potential_origins = copy_list @ predecessors in
		let max_fanIn_nodes = (find_max_fanIn_nodes potential_origins) in
		let origin = match max_fanIn_nodes with
				[] ->	raise (No_available_origins ("Node " ^ (to_string node) ^ " has no potential origin nodes with max fanIn value!"))
			|	[single_node] -> begin 
					IFDEF VERBOSE THEN
						(print_to_file file_buffer ((to_string single_node) ^ " chosen with max fanIn value")) 
					END;
					single_node
				end
			| (head :: tail) as origins -> 
				begin match (find_min_card_nodes origins) with
						[] ->	raise (No_available_origins ("Node " ^ (to_string node) ^ " has no potential origin nodes with min cardinality value!"))
					|	[single_node] -> begin 
							IFDEF VERBOSE THEN
								(print_to_file file_buffer ((to_string single_node) ^ " chosen with min cardinality value")) 
							END;
							single_node
						end
					| (head :: tail) as new_origins -> 
						if copy_list != [] then begin
							let copy_origin = (List.hd copy_list) in
							IFDEF VERBOSE THEN
								(print_to_file file_buffer ((to_string copy_origin) ^ " chosen as a copy")) 
							END;
							copy_origin
						end else
							begin match (find_min_dist_nodes new_origins) with
								[] ->	raise (No_available_origins ("Node " ^ (to_string node) ^ " has no potential origin nodes with min distance value!"))
							|	[single_node] -> begin 
									IFDEF VERBOSE THEN
										(print_to_file file_buffer ((to_string single_node) ^ " chosen with min distance value")) 
									END;
									single_node
								end
							| head :: tail -> head
							end
				end in
		origin

(** Choose provider of port [require] from the list of nodes [nlist]. 
		This is a ligthweight version that picks the first provider at hand 
		without any heuristics. *)
let choose_port_provider file_buffer node require nlist =
	let providers_list = (filter_port_providers require nlist) in	
	match providers_list with 
		[] -> raise (No_available_provider ("No provider available for require " ^ require))
	|	head :: tail -> 
      begin
        (* keep track in provider of the choice made *)      
        (add_node_bound_to_me head require (ref node));     
        (* build an explicit binding to the provider *)      
        let bind_arc = (Bind_arc.make require (ref head)) in       
        (add_bind_arc node bind_arc);
        (require, head)
      end
		
(** Choose provider of port [require] from the list of nodes [nlist], relying 
		on heuristics. *)
(* N.B. we use Bind arcs *)
let choose_port_provider_heuristics file_buffer node require nlist =
	let providers = (filter_port_providers require nlist) in	
	match providers with 
		[] -> raise (No_available_provider ("No provider available for require " ^ require))
	|	_ -> 
      begin
				let max_fanIn_nodes = (find_max_fanIn_nodes providers) in
				let provider = match max_fanIn_nodes with
						[] ->	raise (No_available_providers ("Node " ^ (to_string node) ^ " has no potential providers with max fanIn value!"))
					|	[single_node] -> begin
							IFDEF VERBOSE THEN
								(print_to_file file_buffer ((to_string single_node) ^ " chosen with max fanIn value"))
							END;
							single_node
						end
					| (head :: tail) as same_fanIn_providers -> 
						begin match (find_min_card_nodes same_fanIn_providers) with
								[] ->	raise (No_available_providers ("Node " ^ (to_string node) ^ " has no potential providers with min cardinality value!"))
							|	[single_node] -> begin
									IFDEF VERBOSE THEN
										(print_to_file file_buffer ((to_string single_node) ^ " chosen with min cardinality value")) 
									END;
									single_node
								end
							| (head :: tail) as same_card_providers -> 
									begin match (find_min_dist_nodes same_card_providers) with
											[] ->	raise (No_available_providers ("Node " ^ (to_string node) ^ " has no potential providers with min distance value!"))
										|	[single_node] -> begin
												IFDEF VERBOSE THEN
													(print_to_file file_buffer ((to_string single_node) ^ " chosen with min distance value")) 
												END;
												single_node
											end
										| head :: tail -> head
									end
						end 
				in
        (* keep track in provider of the choice made *)      
        (add_node_bound_to_me provider require (ref node));     
        (* build an explicit binding to the provider *)      
        let bind_arc = (Bind_arc.make require (ref provider)) in       
        (add_bind_arc node bind_arc);
				(* update accordingly the fanIn field of nodes at the same level *)
				(update_fanIn file_buffer provider nlist);
        (require, provider)
      end

(** Choose providers relying on heuristics. *)
let choose_providers ~heuristics_on file_buffer node nlist =
  let requiresList = (requires_of_node node) in      
  (* choose the right function whether relying on heuristics or not *)
  let choose_provider require = match heuristics_on with
  	true -> (choose_port_provider_heuristics file_buffer node require nlist)
  | false -> (choose_port_provider file_buffer node require nlist)
  in
  let providersList = (List.map choose_provider requiresList) in
  providersList  
  
(* this function computes the cardinality (i.e. the nr. of requires) of a component type and a state *)  
let compute_cardinality state origin_node =
  let current_state_card = (List.length state.requires) in
	let card = current_state_card + origin_node.card in
  card

(** Computes the fanIn value of a single node and sets its corresponding field. *)
let compute_node_fanIn file_buffer requires node =
	let provides = (provides_of_node node) in
	let is_among req_ports prov_port = (List.mem prov_port req_ports) in
	let potential_provides = (List.filter (is_among requires) provides) in
	let fanIn = (List.length potential_provides) in
	(*
	if fanIn = 0 then begin
		(print_to_file file_buffer ("For node " ^ (to_string node)));
		let requires_str = (String.concat " | " requires) in
		(print_to_file file_buffer ("Requires: " ^ requires_str));
		let provides_str = (String.concat " | " provides) in
		(print_to_file file_buffer ("Provides: " ^ provides_str));
		let potential_provides_str = (String.concat " | " potential_provides) in
		(print_to_file file_buffer ("Potential provides: " ^ potential_provides_str));
	end;
	*)
	IFDEF VERBOSE THEN
		let string_repr = ("fanIn[" ^ (to_string node) ^ "] := " ^ (string_of_int fanIn)) in
		(print_to_file file_buffer string_repr)
	END;
	node.fanIn <- fanIn
			
(** Computes the fanIn value of a [selected_nodes] and sets the corresponding field. *)
let compute_fanIn file_buffer prevNodes selected_nodes =
	if !selected_nodes = [] then
		raise No_selected_nodes;
	let all_requires = (requires_of_node_list !selected_nodes) in
	IFDEF VERBOSE THEN
		let string_repr = (String.concat " | " all_requires) in
		(print_to_file file_buffer ("Total requires: " ^ string_repr))
	END;
	(List.iter (compute_node_fanIn file_buffer all_requires) prevNodes)  

let print_fanIn file_buffer nodes =
	if nodes = [] then
		raise Empty_list_of_nodes;
	let to_string_with_fanIn node = ((to_string node) ^ ", fanIn = " ^ (string_of_int node.fanIn)) in
	let string_list = (List.map to_string_with_fanIn nodes) in
	let string_repr = (String.concat " | " string_list) in
	(print_to_file file_buffer ("Nodes with fanIn values:\n" ^ string_repr))

(* it creates a new node with the given pair <T,q> *)
let build_initial resTypeRef =
  let comp_type = !resTypeRef in
  let initialState = (comp_type.automaton).(0) in  
	let initialNode = { 
      res_type = resTypeRef; 
			state = (ref initialState); 
			preds = []; 
			sons = []; 
			copy = None;
			require_arcs = []; 
			bindings = []; 
      bound_to_me = [];
      origin = None; 
			card = 0;
			dist = 0;
			fanIn = 0; 
      copy_index = 0;
      is_final = false 
  } in
  initialNode 


let find_state_by_name automaton name =
	let size = (Array.length automaton) in
	let found_state = (ref None) in
	for i = 0 to (size-1) do
		let current_state = automaton.(i) in 
		let state_name = (current_state.id).value in 
		if (state_name = name) then
			found_state := (Some current_state);  
	done;	
	match !found_state with
	|	None -> raise (State_not_found ("state " ^ name 
										^ " could not be found in the corresponding automaton."))
	|	(Some state) -> state

(** Create a new node with the given pair <T,q> where only the name of state 
q is provided. *)
let build_single_target target_type_state_pair =
	let target_type = (fst target_type_state_pair) in 
	let target_state = (snd target_type_state_pair) in 
	let newNode = { 
      res_type = target_type; 
			state = target_state; 
			preds = []; 
			sons = []; 
			copy = None;
			require_arcs = []; 
			bindings = []; 
      bound_to_me = [];
      origin = None; 
			card = 0;
			dist = 0;
			fanIn = 0; 
      copy_index = 0;
      is_final = false 
  } in
	newNode 

(** Build a list of target nodes from a list of . *)
let build_targets type_state_pairs =
	let targets = (List.map build_single_target type_state_pairs) in
	targets

(** Create a new node with the given pair <T,q>. *) 
let build_from_pair resTypeRef state_id =
  let comp_type = !resTypeRef in
  let new_state = (comp_type.automaton).(state_id.key) in  
	let newNode = { 
      res_type = resTypeRef; 
			state = (ref new_state); 
			preds = []; 
			sons = []; 
			copy = None;
			require_arcs = []; 
			bindings = []; 
      bound_to_me = [];
      origin = None; 
			card = 0;
			dist = 0;
			fanIn = 0; 
      copy_index = 0;
      is_final = false 
  } in
	newNode 

(* it creates a new node with the same fields as the original one *)
let clone node =
	let newNode = { 
      res_type = node.res_type; 
			state = node.state; 
			preds = node.preds; (* TODO: we initialize this field: makes sense? *)
			sons = [];  (* TODO: we don't initialize this field: makes sense? *)
			copy = None; (* we leave this uninitialized *) 
			require_arcs = []; (* we leave this uninitialized ???? *) 
			bindings = []; (* we leave this uninitialized ???? *) 
      bound_to_me = [];
      origin = None; (* we leave this uninitialized *) 
			card = node.card;
			dist = node.dist;
			fanIn = node.fanIn; 
      copy_index = 0;
      is_final = false 
  } in
	newNode 
  
(* this function simply builds a new replica R of the given node list "nlist" 
where each node is set as a copy of the corresponding one in "nlist" *)
(* TODO: preds of a copy are different (same pair, different level)  than the ones of their father *)
let build_copy_nodes nlist =
  let copyNodes = (ref []) in
  let build_copy_node node =
    let newNode = (clone node) in
    newNode.copy <- (Some (Copy_arc.make (ref node)));
    newNode.origin <- (Some (ref node));
    newNode.copy_index <- (node.copy_index + 1);
    copyNodes := newNode :: !copyNodes; 
  in
	(List.iter build_copy_node nlist);
	!copyNodes

(* this function computes and returns a list of nodes that are the successors of
 * the given one *)
let build_succs node = 
  let successorsList = (ref [])  in
  let resType = node.res_type and currState = !(node.state) in
	let successors_ids = currState.successors in
	let build_a_succ comp_type succ_state_id = 
			begin
				let succNode = (build_from_pair comp_type succ_state_id) in
				(* node is one of the "fathers" of each successor *)
        let predArc =  (Pred_arc.make (ref node)) in
				succNode.preds <- predArc :: succNode.preds;
				(* successor's distance = father distance + 1 *)
				succNode.dist <- node.dist + 1;
        let succ_state = ((!resType).automaton).(succ_state_id.key) in 
				(* compute successor's cardinality *)
				succNode.card <- (compute_cardinality succ_state node);
				(* finally add current node to the successors' list *)	
				successorsList := succNode :: (!successorsList);
				(*print_endline ("just created node " ^ (to_string succNode)
					^ " with dist. = " ^ (string_of_int succNode.dist) 
					^ " and card. = " ^ (string_of_int succNode.card) 
					^ " and length of preds list = " ^ (string_of_int (List.length succNode.preds)))*)
			end;
	in
	(List.iter (build_a_succ resType) successors_ids);
	!successorsList
	
(* given node list "nlist" it builds a list of the successor nodes that are achievable 
(i.e. whose requires are satisfied by the given "provides") *)
let build_succs_list provides nlist =
  let filter_fulfilled some_provides nodes = 
		(List.filter (fun node -> are_fulfilled (requires_of_node node) some_provides) nodes) in
  let resultNodes = (ref []) in
  List.iter 
  ( fun node -> 
          let successors = (build_succs node) in 
          let new_nodes = (filter_fulfilled provides successors) in 
          resultNodes := new_nodes @ !resultNodes
  )
  nlist;
  !resultNodes

(* this function extracts the head of a list modifying the given list to contain only the tail *)	
let extract nlistRef =
	let nlist = !nlistRef in
	match nlist with
		[] -> raise (Impossible_to_extract_node "list is empty")
	|	head :: tail -> nlistRef := tail; head

let predecessors node =
  let extract_pred_node pred_arc = !(Pred_arc.get_dest pred_arc) in        
  let pred_nodes_list = (List.map extract_pred_node node.preds) in
  pred_nodes_list  

let is_among_predecessors node nodes_list =
  let pred_node_lists = (List.map predecessors nodes_list) in
  let pred_nodes_single_list = (List.concat pred_node_lists) in
  (in_list node pred_nodes_single_list)

let no_successors next_nodes node =
  not (is_among_predecessors node next_nodes)     

let is_copy_of potential_copy node =
  match node.copy with
    None -> false
  | (Some copy_arc) -> let copy_node = !(Copy_arc.get_dest copy_arc) in
                       (pair_eq potential_copy copy_node) 
  
let is_among_copies nodes node =
  List.exists (is_copy_of node) nodes          

let not_copied next_nodes node =
  not (is_among_copies next_nodes node)     

(* a node is final if it has no successor and it is not a copy of somebody else *)
(* N.B. as a side-effect field is_final of node is updated if it is final *)
let is_final_node next_nodes node =
  let result = (no_successors next_nodes node) 
                && (not_copied next_nodes node) in
  if result = true then 
    node.is_final <- true;
  result  

(* given a list of nodes it returns the list of nodes that are final *)  
let get_finals next_nodes nodes =
  (List.filter (is_final_node next_nodes) nodes)

(* compute the path to the origin following recursively the "origin" field *)
let rec find_full_path_to_root node =
  match node.origin with 
    None -> [node]
  | (Some origin_ref) -> node :: (find_full_path_to_root !origin_ref) 


(* this function updates the origin field of all the nodes referenced to a new one *)
(* N.B. we must also update at the same time the sons field *)
let change_origin_list nref_list new_origin_ref =
  let change_origin node_ref = 
    begin
      let node = !node_ref in
      let old_origin = (get_origin_node node) in
      (* among sons choose only the ones that are not a copy *)
      if (pair_not_eq old_origin node) then
      begin
				(*        
        print_string ("\n node " ^ (to_string node)
        ^ " with old origin " ^ (to_string old_origin)
        ^ " is updated to new origin ");
				*)
        node.origin <- (Some new_origin_ref);
        (* update the sons field of origin node *)
        let new_origin = !new_origin_ref in     
        (add_son new_origin node_ref)
				(*
        let new_origin = (get_origin_node node) in
        print_endline (to_string new_origin)
				*)
      end  
    end
  in
  (List.iter change_origin nref_list)  

(* TODO: comment and check correctness *)
let move_bind_arc old_dest new_dest port_nref_pair =
  let port = (fst port_nref_pair) in
  let node_to_change = !(snd port_nref_pair) in
  let bindings = node_to_change.bindings in
  let bind_arc = (Bind_arc.find_by_port_dest bindings port old_dest) in
  (Bind_arc.set_dest bind_arc (port, new_dest))

(* TODO: comment and check correctness *)
let move_incoming_bind_arcs port_nref_pair_list old_dest new_dest =
  (List.iter (move_bind_arc old_dest new_dest) port_nref_pair_list)

 (* compute the path to the origin following recursively the "origin" field *)
let rec find_trim_path_to_root node =
  match node.origin with 
    None -> [node]
  | (Some origin_ref) -> 
    begin
      let origin = !origin_ref in
      (* we add it to the path only if it's not a copy *)
      if (pair_not_eq origin node) then
        node :: (find_trim_path_to_root origin) 
      else begin
      (* move "incoming origin arcs" from current node to origin *)
      (change_origin_list node.sons origin_ref);
      (* move incoming Bind arcs from current node to origin *)
      (move_incoming_bind_arcs node.bound_to_me (ref node) origin_ref);
      (* continue with the search *)
      (find_trim_path_to_root origin)
      end  
    end

(* used to initialize instance line ID, something like "a : A" where is the
 * index given in input *)
let make_id nlist =
  let first_node = (List.hd nlist) in
  let comp_type = !(first_node.res_type) in
  let instance_name = (String.lowercase comp_type.cname) in
  let inst_id = instance_name in
  (*
  let inst_id = (String.concat " " [instance_name; ":"; comp_type.name]) in
  *)
  inst_id 

let extract_comp_type nlist =
  let first_node = (List.hd nlist) in
  let comp_type = first_node.res_type in
  comp_type
			
(** Extract the minimum value from a list of values *)
let rec find_min guess values =
	match values with
		[] -> guess
	| head :: tail ->
			if head < guess then
				(find_min head tail)
			else
				(find_min guess tail)

(** Find the minimum cardinality among the predecessors of [node]. *)
let find_min_cardinality node =
	let pred_nodes = (List.map (fun arc -> !(Pred_arc.get_dest arc)) node.preds) in
	let cardinalities = (List.map (fun node -> node.card) pred_nodes) in
	let first_cardinality = (List.hd cardinalities) in
	let min_cardinality = (find_min first_cardinality cardinalities) in
	min_cardinality
			
(** Find the minimum distance among the predecessors of [node]. *)
let find_min_distance node =
	let pred_nodes = (List.map (fun arc -> !(Pred_arc.get_dest arc)) node.preds) in
	let distances = (List.map (fun node -> node.dist) pred_nodes) in
	let first_distance = (List.hd distances) in
	let min_distance = (find_min first_distance distances) in
	min_distance

(** Elect a single representative for all successors with same pair <T,q>. *)
let compactify nodes =
	let pred_arcs_list = (ref []) in
  List.iter 
  ( fun node -> pred_arcs_list := node.preds @ !pred_arcs_list )
  nodes;
	(* as a representative we simply pick the first node and update its preds field *)
	let representative_node = (List.hd nodes) in
	representative_node.preds <- !pred_arcs_list;
	(* set cardinality to the minimum cardinality value among all predecessors *)
	let min_cardinality = (find_min_cardinality representative_node) in
	representative_node.card <- min_cardinality + (List.length representative_node.require_arcs);
	(* if representative is not a copy, set distance to the minimum distance value among all predecessors *)
	(*
	if (List.exists is_a_copy nodes) then 
		let copied_node = (List.find is_a_copy nodes) in
		let copy_node = (get_copy_node copied_node) in
		representative_node.dist <- copy_node.dist
	else
		representative_node.dist <- (find_min_distance representative_node);
	*)
	try
		let copied_node = (List.find is_a_copy nodes) in
		let copy_node = (get_copy_node copied_node) in
		representative_node.dist <- copy_node.dist;
		representative_node;
	with Not_found -> 
		representative_node.dist <- (find_min_distance representative_node);
		representative_node

(** Take a list with repeated nodes and take a single successor for each group 
		of nodes that are equal w.r.t. <T,q>. *)
let unify_successors nodes =
	let unified_nodes = (ref []) in
	for i = 0 to ((List.length nodes) - 1) do
		let node = (List.nth nodes i) in
			(* if I didn't take this node into account *)
			if (not_in_list node !unified_nodes) then
				let all_successors = (List.filter (pair_eq node) nodes) in
				(* elect a single representative for all successors with same pair <T,q> *)
				let single_representative = (compactify all_successors) in
				unified_nodes := single_representative :: !unified_nodes 
	done;
	!unified_nodes

(** Check to see if [node] is reachable by a new path w.r.t. the corresponding 
		node in current nodes. *)
(*
let has_new_path node current_nodes =
	let same_node = (List.find (pair_eq node) current_nodes) in
	if (List.length node.preds) != (List.length same_node.preds) then
		true
	else
		false
*)
        
end


(********************************************************)
(*			                Arcs type                 			*)
(********************************************************)

    and Pred_arc : sig
        type t
        val get_dest : t -> Node.t ref 
        val set_dest : t -> Node.t ref -> unit
        val make : Node.t ref -> t
        val to_string : t -> string
    end = struct
        type t = { mutable dest : Node.t ref }
        let get_dest arc = arc.dest
        let set_dest arc new_dest = arc.dest <- new_dest
        let make noderef = 
         let predarc = { dest = noderef } in
         predarc
        let to_string arc = (" ==> " ^ (Node.to_string !(arc.dest)))
    end        
    
    and Copy_arc : sig
        type t
        val get_dest : t -> Node.t ref 
        val set_dest : t -> Node.t ref -> unit
        val make : Node.t ref -> t
        val to_string : t -> string
    end = struct
        type t = { mutable dest : Node.t ref }
        let get_dest arc = arc.dest
        let set_dest arc new_dest = arc.dest <- new_dest
        let make noderef = 
         let copyarc = { dest = noderef } in
         copyarc
        let to_string arc = (" --#-- " ^ (Node.to_string !(arc.dest)))
    end        
    
    and Req_arc : sig
        type t
        val get_dest : t -> port_name 
        val set_dest : t -> port_name -> unit
        val make : port_name -> t
        val to_string : t -> string
    end = struct
        type t = { mutable dest : port_name }
        let get_dest arc = arc.dest
        let set_dest arc new_dest = arc.dest <- new_dest
        let make portname = 
         let req_arc = { dest = portname } in
         req_arc
        let to_string arc = (" ----> " ^ arc.dest)
    end        
   
    and Bind_arc : sig
        type t
        val get_dest : t -> port_name * (Node.t ref) 
        val set_dest : t -> (port_name * (Node.t ref)) -> unit
        val make : port_name -> Node.t ref -> t
        val to_string : t -> string
        val find_by_port_dest : t list -> port_name -> Node.t ref -> t
    end = struct
        type t = { mutable dest : port_name * (Node.t ref) }
        let get_dest arc = arc.dest
        let set_dest arc new_dest = arc.dest <- new_dest
        let make portname noderef = 
         let bind_arc = { dest = (portname, noderef) } in
         bind_arc
        let to_string arc = 
          let port = (fst arc.dest) and noderef = (snd arc.dest) in        
          (" ---" ^ port ^ "---> " ^ (Node.to_string !noderef))
       
        let eq_port_ndest a_port a_node_dest arc =      
          let arc_content = arc.dest in
          let port = (fst arc_content) and nref = (snd arc_content) in
          (port = a_port) && (!nref == !a_node_dest)       
          
        let find_by_port_dest arc_list portName node_dest =
          (List.find (eq_port_ndest portName node_dest) arc_list)

    end        
    
  end        

