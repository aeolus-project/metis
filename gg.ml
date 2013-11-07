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

(*	TODO:
 *   we keep a whole state in each node: it suffices to store a state_id and 
 *   then retrieve the whole state from the component and the state_id
 *   
 *   rename field res_type to comp_type 
 *   
 *)

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
      val to_string_full : t -> string
      val to_string_list : t list -> string
      val to_string_list_of_list : (t list) list -> string
      val to_string_list_full : t list -> string
      val print_list : t list -> unit
      val print_list_full : t list -> unit
      
			val build_succs_list : port_name list -> t list -> t list 
      val build_succs_list_DEBUG : port_name list -> t list -> t list 
      
			(* it creates an initial node  < T, q0 > *)
      val build_initial : (component_t ref) -> t
      
			(* it creates target node *)
      val build_target : (component_t ref) -> string -> t
      
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
      
			(* TODO: implement heuristics for this choice *)
      val choose_origin: t -> t
      
			(* from a given node n compute a list of nodes satsfying requirements of n *)
      val choose_providers : t -> (t list) -> ((port_name * t) list) 
      
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
    
		end = struct
    (** A node of the G-graph  is made of :
	    - a component type 
	    - the current state
	    - a list of predecessor arcs
	    - an (optional) reference to the copy node 
	    - a list of require arcs
	    - a list of binding arcs
			- an origin node (the chosen father)
			- a list of the node's sons
			- a list of the nodes bound to the current one
	    - card --> cardinality, i.e. the nr. of requirements (computed in construction phase)
	    - dist --> distance, i.e. the nr. of state-change actions (computed in construction phase)
      - fanIn --> the nr. of incoming arcs (synthesized during bottom-up traversal)
			- copy_index : ???
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
            
            (* TODO: where is better to place these dummy functions? *)  
            (* Test for list emptiness *)
            let is_empty nlist = (nlist = [])
            (* Test for list emptiness *)
            let not_empty nlist = (not (nlist = []))
 
 
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
  
  (* TODO: comment *) 
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
 
    (* 
  let to_string_nlist nlist =
    let nodes_string_list = (List.map to_string nlist) in
    let string_repr = (String.concat "; " nodes_string_list) in
    string_repr  
  
  let to_string_nlist_full nlist =
    let nodes_string_list = (List.map to_string_full nlist) in
    let string_repr = (String.concat "; " nodes_string_list) in
    string_repr  
   *)       

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
    
 (* TODO: implement heuristics for this choice *)
  let choose_origin node =
    match node.copy with 
      (Some copy_arc) -> !(Copy_arc.get_dest copy_arc)
    | None -> if (not_empty node.preds) then
                let pred_arc = (List.hd node.preds) in   
                !(Pred_arc.get_dest pred_arc)
              else
                raise (No_available_origin_node ("node " ^ (to_string node) ^ " has no origin!" ))

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

(* function that retrieves the ports required by a given node *)
let requires_of_node node =
  let actual_state = !(node.state) in
  actual_state.requires

(* function that retrieves the ports required by a given list of nodes *)
let requires_of_node_list nlist =
	List.concat (List.map requires_of_node nlist)

(* this function filters the nodes from node list "nlist" that satisfy the given "require" *)
let filter_port_providers require nlist =
	(List.filter (fun node -> (is_fulfilled require (provides_of_node node))) nlist)

(* TODO: implement heuristics for this choice *)
(* N.B. we use Bind arcs *)
let choose_port_provider node require nlist =
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

(* TODO: comment*)
let choose_providers node nlist =
  let requiresList = (requires_of_node node) in      
  let choose_provider require = (choose_port_provider node require nlist) in
  let providersList = (List.map choose_provider requiresList) in
  providersList  
  
(* this function computes the cardinality (i.e. the nr. of requires) of a component type and a state *)  
let compute_cardinality state origin_node =
  let current_state_card = (List.length state.requires) in
	let card = current_state_card + origin_node.card in
  card

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
										^ "could not be found in the corresponding automaton."))
	|	(Some state) -> state

(** Create a new node with the given pair <T,q> where only the name of state 
q is provided. *)
let build_target resTypeRef target_name =
  let comp_type = !resTypeRef in
  let new_state = (find_state_by_name comp_type.automaton target_name) in  
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

let build_succs_list_DEBUG provides nlist =
  let filter_fulfilled some_provides nodes = 
		(List.filter (fun node -> are_fulfilled (requires_of_node node) some_provides) nodes) in
  let resultNodes = (ref []) in
  List.iter 
  ( fun node -> 
          let successors = (build_succs node) in 
					print_endline ("\nsuccessors of node " ^ (to_string node) 
						^ ": " ^ (to_string_list successors));
          let new_nodes = (filter_fulfilled provides successors) in 
					let new_nodes_str = (ref "{ }") in
					if (new_nodes != []) then
						new_nodes_str := (to_string_list new_nodes);
					print_endline ("and the fulfilled ones are: " ^ !new_nodes_str);
          resultNodes := new_nodes @ !resultNodes;
					print_endline ("resultNodes updated to: " ^ (to_string_list !resultNodes))
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
let old_is_final_node next_nodes node =
  (no_successors next_nodes node) && (not_copied next_nodes node)

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
			
(** Set-minus operation between two lists. *)	
(*
let rec set_minus list1 list2 =
	match list1 with
		[] -> []
	| head :: tail -> 
			begin
				if (List.memq head list2) then
					(set_minus tail list2)
				else 
					head :: (set_minus tail list2)
				end
*)

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
	(* if representative is not a copy set distance to the minimum distance value among all predecessors *)
	if (List.exists is_a_copy nodes) then 
		let copied_node = (List.find is_a_copy nodes) in
		let copy_node = (get_copy_node copied_node) in
		representative_node.dist <- copy_node.dist
	else
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
   
    (* TODO: change name of the field, store port and destination node
     * separately instead of a pair? *) 
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
          (*
          print_endline ("\n in function eq_port_ndest: want to check if
                  contents of arc " ^ (to_string arc) ^ " are equal to " 
                  ^ "port " ^ a_port 
                  ^ " and destination " ^ (Node.to_string !a_node_dest));
          *)        
          (port = a_port) && (!nref == !a_node_dest)       
          

        let find_by_port_dest arc_list portName node_dest =
          (List.find (eq_port_ndest portName node_dest) arc_list)

    end        
    
  end        

