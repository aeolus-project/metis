
open My_datatypes
open Datatypes_t
(*
open Batteries
*)
open My_loops
open Facade
open Action
open Plan
open Gg

module T =
  struct      

(************************************************************)
(*			          T-graph vertex  type                 			*)
(************************************************************)
    module rec Vertex : sig
      type t
      type vertex_tag_t
      val get_id : t -> string
      val get_tag : t -> vertex_tag_t
      val get_full_id : t -> string
      val get_go_edges : t -> Dep_edge.t list
      val get_return_edges : t -> Dep_edge.t list
			val eq_tag : t -> t -> bool
			val eq_id_tag : t -> t -> bool
      val get_nr_in_edges : t -> int
      val set_nr_in_edges : t -> int -> unit
      val get_succ : t -> t
      val increase_nr_in_edges : t -> unit
      val decrease_nr_in_edges : t -> unit
      val has_no_in_edges : t -> bool
      val is_not_final : t -> bool
      val to_string_with_id : t -> string
      val to_string : t -> string
      val to_string_list : t list -> string
      val to_string_list_list : t list list -> string
      val to_string_list_full : t list -> string
      val print_list : t list -> unit
      val print_simple_list : t list -> unit
      val print_actions : Buffer.t ref -> t list -> unit
      val make_create : state_id_t -> string-> (component_t ref) -> int -> t
      val make_delete : state_id_t -> string-> (component_t ref) -> int -> t
      val make : state_id_t -> state_id_t -> string -> (component_t ref) -> int -> t
      val set_inst_edge : t -> t -> (Gg.Node.t ref) -> unit
      val get_inst_edge : t -> Inst_edge.t 
      val has_inst_edge : t -> bool
      val add_go_edge : t -> Dep_edge.t -> unit
      val add_return_edge : t -> Dep_edge.t -> unit
      val has_successor : t -> bool
      val extract_vertex : (t option) ref -> t
			val chop_list : t list -> int -> t list 
			val remove_final_vertex : t list -> t list
			val compute_all_succs : t -> t list
      val find_in_list_by_state : state_id_t -> t list -> t
      val top_sort : t list -> t list
      val top_sort_DEBUG : t list -> t list
			val synthesize_plan : (t list) ref -> string -> string -> Buffer.t ref -> Plan.t
			val synthesize_plan_DEBUG : (t list) ref -> string -> string -> Buffer.t ref -> Plan.t
			val copy_vertices_until : (t list) ref -> t -> t list -> t list
			val copy_vertices_until_DEBUG : (t list) ref -> t -> t list -> t list
			val find_split_edge : t list -> Dep_edge.t  
			val extract_src_from_tag : t -> state_id_t
			val find_in_list_by_go_edge : Dep_edge.t -> t list -> t
			val find_by_tag : vertex_tag_t -> t list -> t
			val remove_go_edge : t -> Dep_edge.t -> unit
			val find_in_blue_edges_vertices : t -> t list -> (Dep_edge.t * t) list
			val find_position : int ref -> t -> t list -> int 
			(* need the following functions to use Ocamlgraph library *)	
			val compare : t -> t -> int
			val hash : t -> int
			val equal : t -> t -> bool
    end = struct

			exception No_instance_edge of string
      exception No_vertex_value
      exception No_arrival_state
      exception Final_vertex_has_no_arrival_state  
      exception Negative_nr_incoming_edges  of string
      exception Impossible_to_extract_vertex of string 
      exception Abnormal_inst_succ_list  
			exception Vertex_tag_not_admitted of string
			exception Not_a_transition_tag of string
			exception No_red_edges_in_cycle of string
			exception Vertex_not_in_list of string
			exception Position_not_found of string
			exception No_candidates_for_duplication
     
      (* Types for the tag associated to every vertex *)      
      type create_tag_t = Create
      type delete_tag_t = Delete
      type state_tag_t = state_id_t
      (* a tag of the kind (n,m) or (C,0) for creation or (n,D) for destruction *)
      type vertex_tag_t =
        Initial of create_tag_t * state_tag_t
      | Trans of state_tag_t * state_tag_t
      | Final of state_tag_t * delete_tag_t

      let string_of_tag vertex_tag =
        match vertex_tag with
          Initial (Create, state_id) -> "(C," ^ state_id.value ^ ")"  
        | Trans (state_id_1, state_id_2) -> "(" ^ state_id_1.value ^ "," ^ state_id_2.value ^ ")"  
        | Final (state_id, Delete) -> "(" ^ state_id.value ^ ",D)"  

			let finalize_tag vertex_tag = 
        match vertex_tag with
        	Trans (state_id_1, state_id_2) -> Final (state_id_1, Delete) 
        | _ -> raise (Vertex_tag_not_admitted ("found the following tag: " 
								^ (string_of_tag vertex_tag) ^ " which is not allowed!")) 
			
			let extract_tag_src vertex_tag = 
        match vertex_tag with
        	Trans (state_id_1, state_id_2) -> state_id_1 
        | Final (state_id, Delete) -> state_id  
        | _ -> raise (Vertex_tag_not_admitted ("found the following tag: " 
								^ (string_of_tag vertex_tag) ^ " which is not allowed!")) 
			
			let extract_tag_dst_name vertex_tag = 
        match vertex_tag with
          Initial (Create, state_id) -> state_id.value  
        |	Trans (state_id_1, state_id_2) -> state_id_2.value 
        | _ -> raise (Vertex_tag_not_admitted ("found the following tag: " 
								^ (string_of_tag vertex_tag) ^ " which is not allowed!")) 

			let get_transition_from_tag vertex_tag =
        match vertex_tag with
         	Trans (state_id_1, state_id_2) -> (state_id_1.value, state_id_2.value)  
        | _ -> raise (Not_a_transition_tag ("found the following tag: " 
								^ (string_of_tag vertex_tag) ^ " which is not a transition!")) 

      type t = {
        id : string;      
				comp_type_name : string; 
        (* a tag of the kind (0,1) or (C,0) for creation or (i,D) for destruction *)
        mutable tag : vertex_tag_t;
				(* used to keep track of the identity of different duplicates *)
				mutable duplicates_nr : int;
        mutable nr_in_edges : int;
        mutable go_edges : Dep_edge.t list;
        mutable return_edges : Dep_edge.t list;
        mutable inst_edge : Inst_edge.t option;
        (* actions are associated to every vertex while sorting topologically *)      
        mutable actions : Action.t list 
      }

			let get_go_edges vertex = vertex.go_edges
			
			let get_return_edges vertex = vertex.return_edges

			let finalize_vertex_tag vertex =
				let final_tag = (finalize_tag vertex.tag) in
				vertex.tag <- final_tag

			let get_tag vertex =
				vertex.tag

			let extract_src_from_tag vertex =
				(extract_tag_src vertex.tag)

			let compute_all_succs vertex =
				let inst_succ = 
					match vertex.inst_edge with
						None -> []
					| (Some edge) -> [ !(Inst_edge.get_dest edge) ] 
				in
				let go_succs = (List.map Dep_edge.extract_dest_vrtx vertex.go_edges) in
				let return_succs = (List.map Dep_edge.extract_dest_vrtx vertex.return_edges) in
				let all_succs = inst_succ @ go_succs @ return_succs in
				all_succs 
				 
			let is_not_final vertex =
				match vertex.tag with
        	Final (state_id, Delete) -> false 
				| _ -> true
		
			let is_final vertex =
				match vertex.tag with
        	Final (state_id, Delete) -> true
				| _ -> false 
				
			let is_initial vertex =
				match vertex.tag with
        	Initial (Create, state_id) -> true 
				| _ -> false
			
			let is_not_initial vertex =
				match vertex.tag with
        	Initial (Create, state_id) -> false 
				| _ -> true

			let eq_tag v1 v2 =
				(v1.tag = v2.tag)

			let eq_id_tag v1 v2 =
				(v1.id = v2.id) && (v1.tag = v2.tag)
			
			let neq_id_tag v1 v2 =
				not (eq_id_tag v1 v2)
			
			(* find the vertex that matches the given one *)  
      let find_in_list vertex vertices =
        (List.find (eq_id_tag vertex) vertices)
			
      let get_actions vertex =
        vertex.actions 

      let set_actions vertex new_actions =
        vertex.actions <- new_actions
      
      let get_id vertex =
        vertex.id
      
      let has_successor vertex =
        match vertex.inst_edge with
          None -> false
        | (Some edge) -> true
      
      let string_of_vtag vertex_tag =
        (string_of_tag vertex_tag)
      
			let get_full_id vertex =
				vertex.id ^ (string_of_vtag vertex.tag)

      let to_string vertex =
        let vtag_str = (string_of_vtag vertex.tag) in
        vtag_str

      let to_string_with_id vertex =
        let str_repr = (vertex.id ^ " " ^ (string_of_vtag vertex.tag)) in
        str_repr

      let string_of_inst_edge vertex =
        match vertex.tag with
          (Final (a_state, Delete)) -> ""
        | _ ->
              begin         
                match vertex.inst_edge with 
                  None -> raise (No_instance_edge ("in vertex " ^ (to_string vertex)))
                | (Some edge) -> (Inst_edge.to_string edge)
              end  

      let to_string_with_inst_edge vertex =
        let vtag_str = (string_of_vtag vertex.tag) in
        let inst_edge_str = (string_of_inst_edge vertex) in
        let string_repr = (vtag_str ^ " " ^ inst_edge_str) in
        string_repr
      
			let to_string_id_inst_edge vertex =
        let vertex_str = (vertex.id ^ " " ^ (string_of_vtag vertex.tag)) in
        let inst_edge_str = (string_of_inst_edge vertex) in
        let string_repr = (vertex_str ^ " " ^ inst_edge_str) in
        string_repr
     
      let to_string_with_id_nr_in_edges vertex =
        let str_repr = (vertex.id ^ " " ^ (string_of_vtag vertex.tag) 
          ^ " nr.IN-edges =  " ^ (string_of_int vertex.nr_in_edges)) in
        str_repr

      let to_string_list vertices =
        let string_list = (List.map to_string_with_id vertices) in
        let string_repr = (String.concat "  |  " string_list) in
        string_repr

      let to_string_list_list vertices_list =
        let string_list_list = (List.map to_string_list vertices_list) in
        let string_repr = (String.concat "\n " string_list_list) in
        string_repr

      let print_simple_list vertices =
        let string_list = (List.map to_string_with_id vertices) in
        let string_repr = (String.concat "  |  " string_list) in
        (print_endline string_repr) 
      
      let print_list vertices =
        let string_list = (List.map to_string_with_id_nr_in_edges vertices) in
        let string_repr = (String.concat "\n" string_list) in
        (print_endline string_repr)

      let actions_to_string actions =
        let string_list = (List.map Action.to_string actions) in
        let string_repr = (String.concat "  " string_list) in
        string_repr

      let to_string_with_actions vertex =  
        let string_repr = (vertex.id ^ " must perform actions: " ^ (actions_to_string vertex.actions)) in
        string_repr

      (* TODO: da finire per una stampa decente del piano 
      (* it builds a list of pairs where each element has attached an index *) 
 
      let enumerate index a_list =
        match a_list with
          [] -> []
        | head :: tail -> 
                          let new_index = (index + 1) in
                          (index, head) :: (enumerate new_index tail) 

      let string_of_index_action_pair index_action_pair =
        let index = (fst index_action_pair) in              
        let action = (snd index_action_pair) in
        let str_repr = ("action nr." ^ (string_of_int index) ^ " : " ^         
      *)

      let print_actions file_buffer vertices =
        let string_list = (List.rev (List.map to_string_with_actions vertices)) in
        let string_repr = (String.concat "\n" string_list) in
        print_endline string_repr;
				(Buffer.add_string !file_buffer (string_repr ^ "\n"))

      let to_string_full vertex =
				let header_str = (to_string_with_id_nr_in_edges vertex) in
        let go_edges_str = (Dep_edge.string_of_list vertex.go_edges) in   
        let return_edges_str = (Dep_edge.string_of_list vertex.return_edges) in   
        let inst_edge_str = (Inst_edge.to_string_opt vertex.inst_edge) in 
        let vertex_str = (header_str ^ "\n GO EDGES: " ^ go_edges_str
					^ "\n RETURN EDGES: " ^ return_edges_str ^ "\n INST EDGE: " 
					^ inst_edge_str) in
        vertex_str 

(*
      let to_string_list vertices_list =
        let string_list = (List.map to_string_with_inst_edge vertices_list) in
        let string_repr = (String.concat " " string_list) in
        string_repr  
*)
      
      let to_string_with_id_list vertices_list =
        let string_list = (List.map to_string_with_id vertices_list) in
        let string_repr = (String.concat " " string_list) in
        string_repr  

      let to_string_list_full vertices_list =
        let string_list = (List.map to_string_full vertices_list) in
        let string_repr = (String.concat "\n\n" string_list) in
        string_repr  

			let get_succ vertex = 
				match vertex.inst_edge with
						None -> raise (No_instance_edge ("in vertex " ^ (to_string_with_id vertex)))
					| (Some edge) -> !(Inst_edge.get_dest edge)
      
			(* a_state should always be (State 0) *)  
      let make_create a_state inst_id comp_type dupl_nr =
        let create_vertex = {
          id = inst_id;      
					comp_type_name = (!comp_type).cname;      
          tag = (Initial (Create, a_state));
					duplicates_nr = dupl_nr;
          nr_in_edges = 0;
          go_edges = [];
          return_edges = [];
          inst_edge = None;
          actions = [];
        } in
        create_vertex

     (* N.B. we already initialize field nr_in_edges to 1 because we know that
      * when we make a vertex of this kind there is an instance edge pointing to
      * it *) 
      let make_delete a_state inst_id comp_type dupl_nr =
        let delete_vertex = {
          id = inst_id;      
					comp_type_name = (!comp_type).cname;      
          tag = (Final (a_state, Delete));
					duplicates_nr = dupl_nr;
          nr_in_edges = 1;
          go_edges = [];
          return_edges = [];
          inst_edge = None;
          actions = [];
        } in
        delete_vertex
      
     (* N.B. we already initialize field nr_in_edges to 1 because we know that
      * when we make a vertex of this kind there is an instance edge pointing to
      * it *)
      let make src_state dst_state inst_id comp_type dupl_nr =
        let new_vertex = {
          id = inst_id;
					comp_type_name = (!comp_type).cname;      
          tag = (Trans (src_state, dst_state));
					duplicates_nr = dupl_nr;
          nr_in_edges = 1;
          go_edges = [];
          return_edges = [];
          inst_edge = None;
          actions = [];
        } in
        new_vertex
      
			let get_inst_edge vertex =
        match vertex.inst_edge with 
          None -> raise (No_instance_edge ("for vertex " ^ (to_string vertex)))
        | (Some edge) -> edge
			
			let has_inst_edge vertex =
        match vertex.inst_edge with 
          None -> false
        | (Some edge) -> true

      let set_inst_edge vertex dst_vertex gnode_ref =
        let new_inst_edge = (Inst_edge.make gnode_ref dst_vertex) in
        vertex.inst_edge <- (Some new_inst_edge) 

      let add_go_edge vertex edge =
        vertex.go_edges <- edge :: vertex.go_edges;
				let dest_vertex = !(Dep_edge.get_dest edge) in
				dest_vertex.nr_in_edges <- (dest_vertex.nr_in_edges + 1)

      let add_return_edge vertex edge =
        vertex.return_edges <- edge :: vertex.return_edges;     
				let dest_vertex = !(Dep_edge.get_dest edge) in
				dest_vertex.nr_in_edges <- (dest_vertex.nr_in_edges + 1)
			
			let remove_go_edge vertex edge =
				vertex.go_edges <- (Dep_edge.remove_edge edge vertex.go_edges);
				let dest_vertex = !(Dep_edge.get_dest edge) in 
				dest_vertex.nr_in_edges <- (dest_vertex.nr_in_edges - 1);
				(print_endline ("Removed GO edge " ^ (to_string_with_id vertex) ^ (Dep_edge.to_string edge)));
				(print_endline ("Destination vertex " ^ (to_string_with_id dest_vertex) ^ " has now nr_in_edges = " ^ (string_of_int dest_vertex.nr_in_edges)))
			
			let remove_return_edge vertex edge =
				vertex.return_edges <- (Dep_edge.remove_edge edge vertex.return_edges);
				let dest_vertex = !(Dep_edge.get_dest edge) in
				dest_vertex.nr_in_edges <- (dest_vertex.nr_in_edges - 1);
				(print_endline ("Removed RETURN vertex " ^ (to_string_with_id vertex) ^ (Dep_edge.to_string edge)));
				(print_endline ("Destination vertex " ^ (to_string_with_id dest_vertex) ^ " has now nr_in_edges = " ^ (string_of_int dest_vertex.nr_in_edges)))

      (* this function simply extracts a value of type Vertex.t from an option *)    
      let extract_vertex opt_vertex_ref =
        match !opt_vertex_ref with
        None -> raise No_vertex_value
        | (Some vertex) -> vertex
          
      (* es. : vertex tag = (3,7) --> 7 *) 
      let get_arrival_state vertex =
        match vertex.tag with
          Initial (Create, a_state) -> a_state  
        | Trans (src_state, dst_state) -> dst_state
        | Final (a_state, Delete) -> raise Final_vertex_has_no_arrival_state  

      let eq_state_vertex astate vertex =
        try
          begin      
          	let vertex_state = (get_arrival_state vertex) in
          	if (astate = vertex_state) then
            	true
          	else
            	false
          end  
        with
          Final_vertex_has_no_arrival_state -> false 
      
      (* find the vertex that matches the given state *)  
      let find_in_list_by_state a_state vertices =
        (List.find (eq_state_vertex a_state) vertices)

			let get_nr_in_edges vertex =
        vertex.nr_in_edges
      
      let set_nr_in_edges vertex value =
        vertex.nr_in_edges <- value
      
      let increase_nr_in_edges vertex =
        vertex.nr_in_edges <- (vertex.nr_in_edges + 1)
      
      let decrease_nr_in_edges vertex =
        if (vertex.nr_in_edges > 0) then
          vertex.nr_in_edges <- (vertex.nr_in_edges - 1)
      
      let ref_decrease_nr_in_edges vertex_ref =
        (decrease_nr_in_edges !vertex_ref) 

      let has_no_in_edges vertex =
        if (vertex.nr_in_edges > 0) then
          false
        else if (vertex.nr_in_edges = 0) then
          true
        else
          raise (Negative_nr_incoming_edges ("vertex " ^ (to_string vertex) 
                  ^ " has a negative nr. of incoming edges!")) 

			(* this function retrievs the last vertex of a list *)
			let get_last_v verts_list =
				let length = (List.length verts_list) in
				let last_vertex = (List.nth verts_list (length-1)) in
				last_vertex
			
			(* function that copies an instance line until a given vertex
			 * to be used in the splitting phase *)
			let copy_vertices_until new_vertices vertex orig_vertices =
				if (List.memq vertex orig_vertices) = false then
					raise (Vertex_not_in_list ("Vertex " ^ (to_string_with_id vertex) 
						^ " could not be found in the following list " 
						^ (to_string_list orig_vertices)));
				let vertices = (Array.of_list orig_vertices) in
				let i = (ref 0) in
				let current_vertex = (ref vertices.(!i)) in
				(*while (!i < length) && (neq_id_tag vertex !next_vertex) do*)
				while (neq_id_tag vertex !current_vertex) do
					let new_vertex = {
						id = (!current_vertex.id ^ "'");
						comp_type_name = !current_vertex.comp_type_name;
          	tag = !current_vertex.tag;
						duplicates_nr = !current_vertex.duplicates_nr;
          	nr_in_edges = 0;
          	go_edges = [];
          	return_edges = [];
          	inst_edge = None; 
          	actions = [];
					} in
					(* if current it's not the initial vertex I need to set inst_edge field of its predecessor *)
					if (is_not_initial !current_vertex) then
						begin
							let last_vertex = (List.hd !new_vertices) in
							(* we need to fetch the same G-node reference of the original inst_edge *)
							let previous_vertex = vertices.(!i-1) in
							let prev_inst_edge = (Inst_edge.extract_value previous_vertex.inst_edge) in
							let prev_gnode_tag = (Inst_edge.get_tag prev_inst_edge) in
							(* we build a new instance edge *)
							(set_inst_edge last_vertex new_vertex prev_gnode_tag); 
							new_vertex.nr_in_edges <- 1 (* TODO move increment into set dest? *)
						end;
					(* finally we add the new vertex *)
					new_vertices := new_vertex :: !new_vertices;
					i := !i + 1;
					current_vertex := vertices.(!i);
				done;
				(* after exiting the loop we still need to add current_vertex: 
				 * for now in a dummy copy-and-pasted loop body
				 * TODO: change it in something less ugly 
				 *)
				let new_vertex = {
					id = (!current_vertex.id ^ "'");
					comp_type_name = !current_vertex.comp_type_name;
          tag = !current_vertex.tag;
					duplicates_nr = !current_vertex.duplicates_nr;
          nr_in_edges = 1;
          go_edges = [];
          return_edges = [];
          inst_edge = None; 
          actions = [];
				} in
				let last_vertex = (List.hd !new_vertices) in
				(* we need to fetch the same G-node reference of the original inst_edge *)
				let previous_vertex = vertices.(!i-1) in
				let prev_inst_edge = (Inst_edge.extract_value previous_vertex.inst_edge) in
				let prev_gnode_tag = (Inst_edge.get_tag prev_inst_edge) in
				(* we build a new instance edge *)
				(set_inst_edge last_vertex new_vertex prev_gnode_tag); 
				(* we must change vertex tag from (s,s') into something like (s,D) *)
				(finalize_vertex_tag new_vertex); 
				new_vertices := new_vertex :: !new_vertices;
				(List.rev !new_vertices)
 	
			let copy_vertices_until_DEBUG new_vertices vertex orig_vertices =
				if (List.memq vertex orig_vertices) = false then
					raise (Vertex_not_in_list ("Vertex " ^ (to_string_with_id vertex) 
						^ " could not be found in the following list " 
						^ (to_string_list orig_vertices)));
				let vertices = (Array.of_list orig_vertices) in
				let i = (ref 0) in
				let current_vertex = (ref vertices.(!i)) in
				(*while (!i < length) && (neq_id_tag vertex !next_vertex) do*)
				while (neq_id_tag vertex !current_vertex) do
					print_endline ("\niteration nr." ^ (string_of_int !i));
					print_endline (" current_vertex = " ^ (to_string_with_id !current_vertex));
					let new_vertex = {
						id = (!current_vertex.id ^ "'");
						comp_type_name = !current_vertex.comp_type_name;
          	tag = !current_vertex.tag;
						duplicates_nr = !current_vertex.duplicates_nr;
          	nr_in_edges = 0;
          	go_edges = [];
          	return_edges = [];
          	inst_edge = None; 
          	actions = [];
					} in
					print_endline (" new_vertex = " ^ (to_string_with_id new_vertex));
					(* if current it's not the initial vertex I need to set inst_edge field of its predecessor *)
					if (is_not_initial !current_vertex) then
						begin
							print_endline "\nsince it's not an initial vertex we need to do some work";
							let last_vertex = (List.hd !new_vertices) in
							(* we need to fetch the same G-node reference of the original inst_edge *)
							let previous_vertex = vertices.(!i-1) in
							let prev_inst_edge = (Inst_edge.extract_value previous_vertex.inst_edge) in
							let prev_gnode_tag = (Inst_edge.get_tag prev_inst_edge) in
							(* we build a new instance edge *)
							(set_inst_edge last_vertex new_vertex prev_gnode_tag); 
							new_vertex.nr_in_edges <- 1
						end;
					(* finally we add the new vertex *)
					new_vertices := new_vertex :: !new_vertices;
					print_endline ("\n new_vertices = { "	^ (to_string_list (List.rev !new_vertices)) ^ " }");
					i := !i + 1;
					current_vertex := vertices.(!i);
				done;
				(* after exiting the loop we still need to add current_vertex: 
				 * for now in a dummy copy-and-pasted loop body
				 * TODO: change it in something less ugly 
				 *)
				let new_vertex = {
					id = (!current_vertex.id ^ "'");
					comp_type_name = !current_vertex.comp_type_name;
          tag = !current_vertex.tag;
					duplicates_nr = !current_vertex.duplicates_nr;
          nr_in_edges = 1;
          go_edges = [];
          return_edges = [];
          inst_edge = None; 
          actions = [];
				} in
				print_endline (" last new_vertex = " ^ (to_string_with_id new_vertex));
				let last_vertex = (List.hd !new_vertices) in
				(* we need to fetch the same G-node reference of the original inst_edge *)
				let previous_vertex = vertices.(!i-1) in
				let prev_inst_edge = (Inst_edge.extract_value previous_vertex.inst_edge) in
				let prev_gnode_tag = (Inst_edge.get_tag prev_inst_edge) in
				(* we build a new instance edge *)
				(set_inst_edge last_vertex new_vertex prev_gnode_tag); 
				(* we must change vertex tag from (s,s') into something like (s,D) *)
				(finalize_vertex_tag new_vertex); 
				new_vertices := new_vertex :: !new_vertices;
				print_endline ("\n Final new_vertices = { "	^ (to_string_list (List.rev !new_vertices)) ^ " }");
				(List.rev !new_vertices)


 
			
			(******************************************************)
			(* 			functions needed to use Ocamlgraph library		*)
      (******************************************************)
			let compare v1 v2 =
				let v1_full_id =  (get_full_id v1)  in
				let v2_full_id =  (get_full_id v2)  in
				(Pervasives.compare v1_full_id v2_full_id)
			let hash = Hashtbl.hash
			let equal = eq_id_tag
		


(******************************************************************)
(*			                  	SPLITTING			                   			*)
(******************************************************************)

			(* find the vertex that matches the given tag *)	
			let rec find_by_tag tag vertices =
				match vertices with	
					[] -> raise Not_found
				|	head :: tail -> 
						begin
							if (head.tag = tag) then
								head
							else
								(find_by_tag tag tail)
						end
		
			(* find the vertex that contains the given go_edge *)	
			let rec find_in_list_by_go_edge go_edge vertices =
				let contains_go_edge vertex edge =
					(List.memq edge vertex.go_edges) 
				in
				match vertices with	
					[] -> raise Not_found
				|	head :: tail -> 
						begin
							if (contains_go_edge head go_edge) then
								head
							else
								(find_in_list_by_go_edge go_edge tail)
						end

			(* an edge is right if its destination vertex:
			 *   1. is among the other vertices in the cycle
			 *   2. is not final (we want to avoid choosing (q3,D) 
			 *)
			let is_right_split_edge edge vertices =
				let dst_vertex = !(Dep_edge.get_dest edge) in
					if (List.memq dst_vertex vertices) && (is_not_final dst_vertex) then
						true
					else
						false

			(* among all red edges of "vertex" find a good candidate for splitting *)
			let find_split_e vertex vertices =
				let rec aux_fun edges =
					match edges with
						[] -> None
					|	head :: tail -> 
							begin
								if (is_right_split_edge head vertices) then
									(Some head)	
								else
									(aux_fun tail)
							end	
				in	
				(aux_fun vertex.return_edges)

			(* among all edges in a cycle find the right one for splitting *)
			let find_split_edge cycle_verts =
				let rec find_split_aux verts_to_scan =
					match verts_to_scan with
						[] -> raise Not_found
					| head :: tail ->
							begin
								let split_vertex_opt = (find_split_e head cycle_verts) in
								match split_vertex_opt with
									None -> (find_split_aux tail)
								| (Some split_edge) -> split_edge
							end
				in
				(find_split_aux cycle_verts) 

			let find_in_blue_edges_vertices vertex vertices =
				let vertices_array = (Array.of_list vertices) in 
				let length = (Array.length vertices_array) in
				let matchings = (ref []) in
				for j = 0 to (length - 1) do
					begin
						let current_vertex = vertices_array.(j) in
						let matching_pairs = (ref []) in
						let edges_array = (Array.of_list current_vertex.go_edges) in 
						for i = 0 to ((Array.length edges_array)-1) do
							let edge = edges_array.(i) in
							if (!(Dep_edge.get_dest edge) == vertex) then
								matching_pairs := (edge, current_vertex) :: !matching_pairs; 
						done;
						matchings := !matching_pairs @ !matchings
					end
				done;
				!matchings	

			let rec find_position position vertex vertices =
				match vertices with
					[] -> raise (Position_not_found ("was not able to find position of vertex "
									^ (to_string_with_id vertex) ^ " among " 
									^ (to_string_list vertices))); 
				|	head :: tail ->
						if (eq_id_tag vertex head) then
							!position
						else
							begin
								position := !position + 1;
								(find_position position vertex tail)
							end

			let rec remove_final_vertex vertices =
				match vertices with
					[] -> []
				| [last] -> []
				| head :: tail -> head :: (remove_final_vertex tail) 
						
			let chop_list vertices index =
				let vertices_array = (Array.of_list vertices) in
				let chopped_list = (ref []) in
				for i = (index-1) downto 0 do
					let current_vertex = vertices_array.(i) in
					chopped_list := current_vertex :: !chopped_list 
				done; 
				!chopped_list 

(******************************************************************)
(*			                  TOPOLOGICAL SORT                   			*)
(******************************************************************)
      
      let not_empty vertices =
        (vertices != [])
      
      let add_vertices orig_verts_ref new_vertices =
        (*orig_verts_ref := new_vertices @ !orig_verts_ref *)
        orig_verts_ref := !orig_verts_ref @ new_vertices 

      (* this function extracts the head of a list modifying the given list to contain only the tail *)	
      let extract_from vlistRef =
	      let vlist = !vlistRef in
	      match vlist with
		      [] -> raise (Impossible_to_extract_vertex "list is empty")
	      |	head :: tail -> vlistRef := tail; head
      
      (*****************************************************)
      (*                Dealing with successors            *)  
      (*****************************************************)

      (* retrieve the successor vertex in the instance line if there is none
       * return the empty list *)  
      let get_inst_succ vertex =
        match vertex.inst_edge with 
          None -> []
        | (Some edge) ->
                        let succ = (Inst_edge.get_dest edge) in
                        [succ]

      (* retrieve the successor vertices following go-edges *)
      let get_go_succs vertex =
        let go_succs = (List.map Dep_edge.get_dest vertex.go_edges) in
        go_succs        
      
      (* retrieve the successor vertices following return-edges *)
      let get_return_succs vertex =
        let return_succs = (List.map Dep_edge.get_dest vertex.return_edges) in
        return_succs        

      (* retrieve all successors of a vertex following: inst-edge, go-edges and return-edges *)
      let get_successors vertex =
        let inst_successor = (get_inst_succ vertex) in 
        let go_succs = (get_go_succs vertex) in 
        let return_succs = (get_return_succs vertex) in 
        let all_successors = ((inst_successor @ go_succs) @ return_succs) in
        all_successors

      (**************************************************)
      (*                Dealing with Actions            *)  
      (**************************************************)
      let action_from_tag vertex =
        match vertex.tag with
          Initial (Create, state_id) -> 
              [New (vertex.id, vertex.comp_type_name)] 
        | Trans (state_id_1, state_id_2) -> 
              [State_change (vertex.id, state_id_1.value, state_id_2.value)]  
        | Final (state_id, Delete) -> 
              [(Del vertex.id)] (* TODO: for the moment we leave it unspecified *) 

      let old_actions_from_return_edges vertex =
        let action_from_single_edge edge =
          begin      
            let port = (Dep_edge.get_port edge) in
            let dest_id = (Dep_edge.get_dest_id edge) in     
            let bind_action = (Unbind (port, dest_id, vertex.id)) in
            bind_action
          end
        in  
        let actions = (List.map action_from_single_edge vertex.return_edges) in
        actions 
      (* we only compute unbind actions if vertex is not a final one (i,D) *)
			let actions_from_return_edges vertex =
				let actions = (ref []) in
				if (is_not_final vertex) then
					begin
        		let action_from_single_edge edge =
          		begin      
            		let port = (Dep_edge.get_port edge) in
            		let dest_id = (Dep_edge.get_dest_id edge) in     
            		let bind_action = (Unbind (port, dest_id, vertex.id)) in
            		bind_action
          		end
        		in  
        		actions := (List.map action_from_single_edge vertex.return_edges) 
					end;
        !actions 

      let actions_from_go_edges vertex =
        let action_from_single_edge edge =
          begin
            let port = (Dep_edge.get_port edge) in
            let dest_id = (Dep_edge.get_dest_id edge) in     
            let bind_action = (Bind (port, vertex.id, dest_id)) in
            bind_action
          end  
        in  
        let actions = (List.map action_from_single_edge vertex.go_edges) in
        actions 

      let compute_actions vertex =
        let tag_action = (action_from_tag vertex) in
        let bind_actions = (actions_from_go_edges vertex) in        
        let unbind_actions = (actions_from_return_edges vertex) in
        let all_actions = ((unbind_actions @ bind_actions) @ tag_action) in
        all_actions
               
      (*******************************************************)
      (*                Dealing with Edge Removal            *)  
      (*******************************************************)
      
			let remove_inst_edge vertex =
        let inst_successor = (get_inst_succ vertex) in
        match inst_successor with
          [] -> ()
        | [inst_succ_vertex] -> 
						(decrease_nr_in_edges !inst_succ_vertex); vertex.inst_edge <- None 
        | _ -> raise Abnormal_inst_succ_list
			
      let remove_go_edges vertex =
        let go_successors = (get_go_succs vertex) in
        (List.iter ref_decrease_nr_in_edges go_successors);
				vertex.go_edges <- []      
      
      let remove_return_edges vertex =
        let return_successors = (get_return_succs vertex) in
        (List.iter ref_decrease_nr_in_edges return_successors); 
				vertex.return_edges <- []      

      let remove_edges vertex =
        begin
          (remove_inst_edge vertex);
          (remove_go_edges vertex);
          (remove_return_edges vertex)
        end


      (*********************************************************)
			(* TODO: utility functions to be moved to a better place *)
      (*********************************************************)
		
			(** Set-minus operation between two lists. *)	
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

			let vertex_in_list vertex vertices =
				(List.exists (eq_id_tag vertex) vertices)

			let rec elim_duplicates vertices =
				match vertices with
					[] -> []
				| head :: tail -> 
						begin
							if (List.memq head tail) then
								(elim_duplicates tail)
							else 
								head :: (elim_duplicates tail)
						end
			
			(** This function takes a list of vertices [vlist] and a vertex [vertex] 
					and removes it from the list. 
  		*)
			let rec remove_from_list vertex vlist =
						match vlist with
							[] -> []
						| head :: tail -> 
								begin
									if (eq_id_tag vertex head) then
										tail
									else 
										head :: (remove_from_list vertex tail)
								end
     
			let print_stack stack file_buffer =
				let print_vertex vertex =
					(print_string ((to_string_with_id vertex) ^ " | ")); 
					(Printf.bprintf !file_buffer "%s" ((to_string_with_id vertex) ^ " | "))
				in 
				(print_string "STACK = ");
				(Printf.bprintf !file_buffer "%s\n" "STACK = ");
				if (Stack.is_empty stack) then begin
					(Printf.bprintf !file_buffer "[]");
					(print_string "[]\n")
				end else begin
				(Stack.iter print_vertex stack);
				(print_endline "")
				end;
				(Printf.bprintf !file_buffer "%s\n" "")


      (**************************************************)
      (*                  	Duplication		              *)  
      (**************************************************)

			
			(** Mark vertices that are reached through an instance edge or a go/blue edge. *)	
			let mark_wrong marked_vertices vertex =
				let inst_and_go_succs_refs = (get_inst_succ vertex) @  (get_go_succs vertex) in
				let inst_and_go_succs = (List.map (fun x -> !x) inst_and_go_succs_refs) in
				marked_vertices := (elim_duplicates (!marked_vertices @ inst_and_go_succs)) 
				
			(** Find vertex to duplicate. 
					Choose among the list of [vertices] with only incoming return/red edges. *) 
			let find_duplicate_vertex vertices file_buffer =
				let marked_vertices = (ref []) in
				(* TODO: do we need an explicit loop or can we just iterate? need to ask an Ocaml programmer
				(List.iter (mark_wrong marked_vertices) !vertices);
				*)
				let vertices_array = (Array.of_list !vertices) in
				for i = 0 to ((Array.length vertices_array) - 1) do
					let current_vertex = vertices_array.(i) in
					(mark_wrong marked_vertices current_vertex)
				done;
				let final_vertices = (List.filter is_final !vertices) in
				let wrong_vertices = final_vertices @  !marked_vertices in
				let candidates = (set_minus !vertices wrong_vertices) in
				if candidates = [] then
					raise No_candidates_for_duplication
				else 
					(print_endline ("The list of candidates is the following one: " ^ (to_string_list candidates)));
					(Printf.bprintf !file_buffer "%s\n" ("The list of candidates is the following one: " ^ (to_string_list candidates)));
					let duplicate_vertex = (List.hd candidates) in
					duplicate_vertex

			(** Forge a new instance ID from an original one plus the number of times it has been duplicated. *)
			let compute_id original_id duplicates_nr = 
				let new_id = ref original_id in
				for i = 0 to (duplicates_nr - 1) do
					new_id := !new_id ^ "'"
				done;
				!new_id 

			(** Update field [duplicates_nr] in all successors of a given [vertex]. *)
			let rec update_dupl_nr_succs vertex	updated_value =
        match vertex.inst_edge with 
          None -> ()
        | (Some edge) ->
						begin
        			let successor = !(Inst_edge.get_dest edge) in
            	successor.duplicates_nr <- updated_value;
							(update_dupl_nr_succs successor	updated_value)
						end
			
			(** Update field [duplicates_nr] of the given [vertex] and all its successors. *)
			let update_duplicates_nr vertex =
				let updated_value = vertex.duplicates_nr + 1 in
				vertex.duplicates_nr <- updated_value;
				(update_dupl_nr_succs vertex updated_value)

			(** Build an instance (vertex) that is a copy of the original one [vertex]. *)
			let make_dupl_instance vertex	=
				(* update nr of duplicates in original vertex *)
				(update_duplicates_nr vertex);
				(* synthesize the instance ID by using also the duplicates nr *)
				let inst_id = (compute_id vertex.id (vertex.duplicates_nr)) in
				let inst_src_state = (extract_tag_src vertex.tag) in  
        let new_vertex = {
          id = inst_id;      
					comp_type_name = vertex.comp_type_name;      
          tag = (Final (inst_src_state, Delete));
					duplicates_nr = vertex.duplicates_nr;
          nr_in_edges = 0;
          go_edges = [];
          return_edges = [];
          inst_edge = None;
          actions = [];
        } in
				new_vertex

			(** Among all return edges of [src_vertex] find the one, if there is, 
					that points to [dst_vertex]. *)
			(* TODO: there could be more than one return edge pointing to the same dst_vertex *)
			let find_return_edge src_vertex dst_vertex =
				let points_to edge = (dst_vertex == !(Dep_edge.get_dest edge)) in
				try
					let result_edge = (List.find points_to src_vertex.return_edges) in
					(Some result_edge)
				with
				| Not_found -> None

			(** Among [vertices] find all the ones (and corresponding return edges) that 
					point to [vertex]. *)
			(* TODO: there could be more than one return edge pointing to the same dst_vertex *)
			let rec find_return_predecessors vertex vertices =
				match vertices with
					[] -> []
				| head :: tail -> 
						begin
							let return_edge = (find_return_edge head vertex) in
							match return_edge with 
								(Some edge) ->
									edge :: (find_return_predecessors vertex tail)
							|	None -> 
									(find_return_predecessors vertex tail)
						end

			(** Return edge is redirected towards [new_vertex]. *)
			let manage_return_edges duplicate_vertex new_vertex to_visit_stack file_buffer points_to_edge =
				(Dep_edge.set_dest points_to_edge (ref new_vertex));
				duplicate_vertex.nr_in_edges <- (duplicate_vertex .nr_in_edges - 1);
				new_vertex.nr_in_edges <- (new_vertex.nr_in_edges + 1)

			(** Scan the whole [plan] and substitute or add corresponding actions 
					to the original instance that is being duplicated. *)
			let adjust_plan plan orig_inst_ID new_inst_ID =
				for j = ((Plan.length plan) - 1) downto 0 do
					let action = (Plan.get_action plan j) in
					match action with
  				| Bind (port, provider, requirer) when provider = orig_inst_ID ->
																			(Plan.set_action plan j (Bind (port, new_inst_ID, requirer)))
  				| Bind (port, provider, requirer) when requirer = orig_inst_ID ->
  																		(Plan.insert_action plan j (Bind (port, provider, new_inst_ID)))
  				| Unbind (port, provider, requirer) when provider = orig_inst_ID ->
																			(Plan.set_action plan j (Unbind (port, new_inst_ID, requirer)))
  				| Unbind (port, provider, requirer) when requirer = orig_inst_ID ->
  																		(Plan.insert_action plan j (Unbind (port, provider, new_inst_ID)))
	  			|	New (inst_ID, comp_type_name) when inst_ID = orig_inst_ID -> 
																			(Plan.insert_action plan j (New (new_inst_ID, comp_type_name))) 
  				| State_change (inst_ID, src, dst) when inst_ID = orig_inst_ID -> 
																			(Plan.insert_action plan j (State_change (new_inst_ID, src, dst)))
  				| _ -> ()
				done

			(** Find a vertex with only return/red incoming edges and copy it. *)
			let duplicate vertices plan to_visit_stack file_buffer =
				(* find instance to duplicate *)
				let duplicate_vertex = (find_duplicate_vertex vertices file_buffer) in
				(print_endline ("\nChosen vertex for splitting = " ^ (to_string_with_id duplicate_vertex)));
				(Printf.bprintf !file_buffer "%s\n" ("Chosen vertex for splitting = " ^ (to_string_with_id duplicate_vertex)));
				(* build duplicate vertex *)
				let new_vertex = (make_dupl_instance duplicate_vertex) in
				(print_endline ("\nBuilt the following new vertex = " ^ (to_string_with_id new_vertex)));
				(Printf.bprintf !file_buffer "%s\n" ("Built the following new vertex = " ^ (to_string_with_id new_vertex)));
				(* add new vertex to vertices list *)
				vertices := new_vertex :: !vertices;
				(* for all vertices pointing to <i,x,y> move return/red edge to new instance <i',x,e> *)
				let points_to_return_edges = (find_return_predecessors duplicate_vertex !vertices) in
				(List.iter (manage_return_edges duplicate_vertex new_vertex to_visit_stack file_buffer) points_to_return_edges);
				(* fix plan to deal with new instance i' *)
				let orig_inst_id = duplicate_vertex.id in
				let new_inst_id = new_vertex.id in
				(adjust_plan plan orig_inst_id new_inst_id);
				(* return duplicate vertex *)
				duplicate_vertex


			(**************************************************)
      (*                  	Plan Synthesis              *)  
      (**************************************************)
  		
      
			(** Deal with [initial vertices] (i.e. with no incoming edges) *)
			let add_initial_vertex stack plan file_buffer vertex =
				let new_action = (New (vertex.id, vertex.comp_type_name)) in
				(Plan.add plan new_action);
				(Printf.bprintf !file_buffer "%s\n" ("Added action " ^ (Action.to_string new_action) ^ " to the plan."));	
				(Stack.push vertex stack)

			(** Deal with [return edges] (the red ones) *)
			let process_ret_edge plan stack src_vertex file_buffer return_edge =
				let dst_vertex = !(Dep_edge.get_dest return_edge) in
				let port = (Dep_edge.get_port return_edge) in
				let unbind_act = Unbind (port, dst_vertex.id, src_vertex.id) in (* unbind action is performed by requirer *)
				(* add unbind action to plan *)
				(Plan.add plan unbind_act);
				(* remove edge *)
				(remove_return_edge src_vertex return_edge);
				(* if destination vertex has no more incoming edges push it onto stack toVisit *)
				if (has_no_in_edges dst_vertex) then begin
					(* 
					(print_endline ("RETURN Edge: PUSH vertex: " ^ (to_string_with_id dst_vertex))); 
					*)
					(Printf.bprintf !file_buffer "%s\n" ("RETURN Edge: PUSH vertex: " ^ (to_string_with_id dst_vertex))); 
					(Stack.push dst_vertex stack);
					(print_stack stack file_buffer)
				end

			(** Deal with [go edges] (the blue ones) *)
			let process_go_edge plan stack src_vertex file_buffer go_edge =
				let dst_vertex = !(Dep_edge.get_dest go_edge) in
				let port = (Dep_edge.get_port go_edge) in
				let bind_act = Bind (port, src_vertex.id, dst_vertex.id) in (* bind action is performed by provider *)
				(* add bind action to plan *)
				(Plan.add plan bind_act);
				(* remove edge *)
				(remove_go_edge src_vertex go_edge);
				(* if dest. vertex has no more incoming edges push it onto stack toVisit *)
				if (has_no_in_edges dst_vertex) then begin
					(Printf.bprintf !file_buffer "%s\n" ("GO Edge: PUSH vertex: " ^ (to_string_with_id dst_vertex))); 
					(Stack.push dst_vertex stack);
					(print_stack stack file_buffer)
				end

			(** Deal with the [instance edge] *)
			let process_inst_edge plan stack file_buffer vertex =
				if (is_not_final vertex) then begin
					let successor = (get_succ vertex) in
					(remove_inst_edge vertex);
					if (has_no_in_edges successor) then begin
						(Printf.bprintf !file_buffer "%s\n" ("INST Edge: PUSH vertex: " ^ (to_string_with_id successor))); 
						(Stack.push successor stack);
					(print_stack stack file_buffer)
					end
				end
			
			(** Compute a state change action corresponding to a vertex *)
			let compute_state_change_act vertex =
				let id = vertex.id in
				let trans_pair = (get_transition_from_tag vertex.tag) in
				let src_state = (fst trans_pair) in
				let dst_state = (snd trans_pair) in
				let state_change_act = State_change (id, src_state, dst_state) in 
				state_change_act 
			
			
			(** Compute a deployment plan *)
			let synthesize_plan vertices targetComponent targetState file_buffer =
				(* initialize data structures *)
				let plan = (Plan.make 100) in 
				let toVisit = Stack.create () in
				let finished = (ref false) in
        (* all initial vertices are pushed on the toVisit stack *)
				let startVertices = (List.filter has_no_in_edges !vertices) in
					(List.iter (add_initial_vertex toVisit plan file_buffer) startVertices);
				(* External loop *)
				(repeat_until 
					(* External loop body *)
					(fun i ->
          	begin
							(Printf.bprintf !file_buffer "%s\n" ("External loop iteration i = " ^ (string_of_int !i)));
							i := !i + 1;
							(* Inner loop *)
							(repeat_until 
								(* Inner loop body *)
								(fun j ->
          				begin 
										(Printf.bprintf !file_buffer "\n*********************** %s\n" ("Internal loop iteration j = " ^ (string_of_int !j)));
										(Printf.bprintf !file_buffer "%s\n" ("Plan BEFORE: " ^ (Plan.to_string plan)));
										j := !j + 1;
										let currentVertex = (Stack.pop toVisit) in
										(Printf.bprintf !file_buffer "%s\n" ("Vertex popped: " ^ (to_string_with_id currentVertex))); 
										(print_stack toVisit file_buffer);
										(* initial node case *)
										if (is_initial currentVertex) then begin
											(* deal with instance successor *)
											(Printf.bprintf !file_buffer "%s\n" "Deal with successor vertex.");
											(process_inst_edge plan toVisit file_buffer currentVertex)
										(* final node case *)
										end else if (is_final currentVertex) then begin
											(* deal with return/red edges *)
											(Printf.bprintf !file_buffer "%s\n" "Deal with return/red edges");
											(List.iter (process_ret_edge plan toVisit currentVertex file_buffer) currentVertex.return_edges);
											(* add del action *)
											(Printf.bprintf !file_buffer "%s\n" "Current vertex is final: we add a Del action to the plan.");
											let deleteAct = (Del currentVertex.id) in
											(Plan.add plan deleteAct)
										(* inner node case *)
										end else begin
											(* add stateChange action *)
											let stateChangeAct = (compute_state_change_act currentVertex) in
											(Plan.add plan stateChangeAct);
											(Printf.bprintf !file_buffer "%s\n" ("It's an intermediate vertex => add action " 
													^ (Action.to_string stateChangeAct) ^ " to the plan."));
											(* deal with go/blue edges *)
											(Printf.bprintf !file_buffer "%s\n" "Deal with go/blue edges");
											(List.iter (process_go_edge plan toVisit currentVertex file_buffer) currentVertex.go_edges);
											(* deal with return/red edges *)
											(Printf.bprintf !file_buffer "%s\n" "Deal with return/red edges");
											(List.iter (process_ret_edge plan toVisit currentVertex file_buffer) currentVertex.return_edges);
											(* deal with instance successor *)
											(Printf.bprintf !file_buffer "%s\n" "Deal with successor vertex.");
											(process_inst_edge plan toVisit file_buffer currentVertex)
										end;
										(* if we reach the target node *)
										if (currentVertex.comp_type_name = targetComponent) && 
												((extract_tag_dst_name currentVertex.tag) = targetState) then 
											begin 
													(Printf.bprintf !file_buffer "%s\n" "Target has been REACHED.");
													finished := true
											end;
										(* delete current vertex from vertices list *)
										vertices := (remove_from_list currentVertex !vertices); 
										(Printf.bprintf !file_buffer "%s\n" ("Vertex removed: " ^ (to_string_with_id currentVertex))); 
										j
          				end)
								(* Inner loop condition: stop when we reach a fixpoint (no new nodes are added) or we find target *)
								(fun j -> ((Stack.is_empty toVisit) || !finished)) 
      					~init:(ref 0));
							(* If we finished without reaching target => there are unvisited 
									vertices (couldn't be visited in topological order) => need duplication *)
							if !finished = false then begin
								(Printf.bprintf !file_buffer "%s\n" "\n ************************* NEED INSTANCE DUPLICATION *************************** "); 
								let vertex = (duplicate vertices plan toVisit file_buffer) in begin
									if vertex.nr_in_edges = 0 then begin
										(Stack.push vertex toVisit);
										(Printf.bprintf !file_buffer "%s\n" ("Pushed duplicated vertex: " ^ (to_string_with_id vertex)));
										(print_stack toVisit file_buffer) 
									end; 
								end;
							end;	
							i
          	end)
					(* External loop condition *)
					(fun i -> !finished)
      	~init:(ref 0));
				plan


			(** Compute a deployment plan *)
			let synthesize_plan_old_style vertices targetComponent targetState file_buffer =
				(* initialize data structures *)
				let plan = (Plan.make 100) in 
				let toVisit = Stack.create () in
				let finished = (ref false) in
        (* all initial vertices are pushed on the toVisit stack *)
				let startVertices = (List.filter has_no_in_edges !vertices) in
					(List.iter (add_initial_vertex toVisit plan file_buffer) startVertices);
				(* External loop *)
				(repeat_until 
					(* External loop body *)
					(fun i ->
          	begin
							(Printf.bprintf !file_buffer "%s\n" ("External loop iteration i = " ^ (string_of_int !i)));
							i := !i + 1;
							(* Inner loop *)
							(repeat_until 
								(* Inner loop body *)
								(fun j ->
          				begin 
										(Printf.bprintf !file_buffer "\n*********************** %s\n" ("Internal loop iteration j = " ^ (string_of_int !j)));
										(Printf.bprintf !file_buffer "%s\n" ("Plan BEFORE: " ^ (Plan.to_string plan)));
										j := !j + 1;
										let currentVertex = (Stack.pop toVisit) in
										(Printf.bprintf !file_buffer "%s\n" ("Vertex popped: " ^ (to_string_with_id currentVertex))); 
										(print_stack toVisit file_buffer);
										(* deal with return/red edges *)
										(Printf.bprintf !file_buffer "%s\n" "Deal with return/red edges");
										(List.iter (process_ret_edge plan toVisit currentVertex file_buffer) currentVertex.return_edges);
										if (is_final currentVertex) then begin
											(Printf.bprintf !file_buffer "%s\n" "Current vertex is final: we add a Del action to the plan.");
											let deleteAct = (Del currentVertex.id) in
											(Plan.add plan deleteAct)
										end else begin
											(* deal with go/blue edges *)
											(Printf.bprintf !file_buffer "%s\n" "Deal with go/blue edges");
											(List.iter (process_go_edge plan toVisit currentVertex file_buffer) currentVertex.go_edges);
											if (is_not_initial currentVertex) then begin
													let stateChangeAct = (compute_state_change_act currentVertex) in
													(Plan.add plan stateChangeAct);
													(Printf.bprintf !file_buffer "%s\n" ("It's an intermediate vertex => add action " 
															^ (Action.to_string stateChangeAct) ^ " to the plan."))
												end;
											(* deal with instance successor *)
											(Printf.bprintf !file_buffer "%s\n" "Deal with successor vertex.");
											(process_inst_edge plan toVisit file_buffer currentVertex)
										end;
										(* if we reach the target node *)
										if (currentVertex.comp_type_name = targetComponent) && 
												((extract_tag_dst_name currentVertex.tag) = targetState) then begin 
													(Printf.bprintf !file_buffer "%s\n" "Target has been REACHED.");
													finished := true
											end;
										(* delete current vertex from vertices list *)
										vertices := (remove_from_list currentVertex !vertices); 
										(Printf.bprintf !file_buffer "%s\n" ("Vertex removed: " ^ (to_string_with_id currentVertex))); 
										j
          				end)
								(* Inner loop condition: stop when we reach a fixpoint (no new nodes are added) or we find target *)
								(fun j -> ((Stack.is_empty toVisit) || !finished)) 
      					~init:(ref 0));
							(* If we finished without reaching target => there are unvisited 
									vertices (couldn't be visited in topological order) => need duplication *)
							if !finished = false then begin
								(Printf.bprintf !file_buffer "%s\n" "\n ************************* NEED INSTANCE DUPLICATION *************************** "); 
								let vertex = (duplicate vertices plan toVisit file_buffer) in begin
									if vertex.nr_in_edges = 0 then begin
										(Stack.push vertex toVisit);
										(Printf.bprintf !file_buffer "%s\n" ("Pushed duplicated vertex: " ^ (to_string_with_id vertex)));
										(print_stack toVisit file_buffer) 
									end; 
								end;
							end;	
							i
          	end)
					(* External loop condition *)
					(fun i -> !finished)
      	~init:(ref 0));
				plan

			(** Compute a deployment plan DEBUG version *)
			let synthesize_plan_DEBUG vertices targetComponent targetState file_buffer =
				(* initialize data structures *)
				let plan = (Plan.make 100) in 
				let toVisit = Stack.create () in
				let finished = (ref false) in
        (* all initial vertices are pushed on the toVisit stack *)
				let startVertices = (List.filter has_no_in_edges !vertices) in
					(List.iter (add_initial_vertex toVisit plan file_buffer) startVertices);
				(* External loop *)
				(repeat_until 
					(* External loop body *)
					(fun i ->
          	begin
							(print_endline ("External loop iteration i = " ^ (string_of_int !i)));
							(Printf.bprintf !file_buffer "%s\n" ("External loop iteration i = " ^ (string_of_int !i)));
							i := !i + 1;
							(* Inner loop *)
							(repeat_until 
								(* Inner loop body *)
								(fun j ->
          				begin 
										(print_endline ("\n***************** Internal loop iteration j = " ^ (string_of_int !j)));
										(print_string "VERTICES = "); (print_simple_list !vertices);
										(print_endline ("Plan BEFORE: " ^ (Plan.to_string plan)));
										(Printf.bprintf !file_buffer "\n*********************** %s\n" ("Internal loop iteration j = " ^ (string_of_int !j)));
										(Printf.bprintf !file_buffer "%s\n" ("Plan BEFORE: " ^ (Plan.to_string plan)));
										j := !j + 1;
										let currentVertex = (Stack.pop toVisit) in
										(print_endline ("Vertex popped: " ^ (to_string_with_id currentVertex))); 
										(Printf.bprintf !file_buffer "%s\n" ("Vertex popped: " ^ (to_string_with_id currentVertex))); 
										(print_stack toVisit file_buffer);
										(* deal with return/red edges *)
										(print_endline "Deal with return/red edges");
										(Printf.bprintf !file_buffer "%s\n" "Deal with return/red edges");
										(List.iter (process_ret_edge plan toVisit currentVertex file_buffer) currentVertex.return_edges);
										if (is_final currentVertex) then begin
											(print_endline "Current vertex is final: we add a Del action to the plan.");
											(Printf.bprintf !file_buffer "%s\n" "Current vertex is final: we add a Del action to the plan.");
											let deleteAct = (Del currentVertex.id) in
											(Plan.add plan deleteAct)
										end else begin
											(* deal with go/blue edges *)
											(print_endline "Deal with go/blue edges");
											(Printf.bprintf !file_buffer "%s\n" "Deal with go/blue edges");
											(List.iter (process_go_edge plan toVisit currentVertex file_buffer) currentVertex.go_edges);
											if (is_not_initial currentVertex) then begin
													let stateChangeAct = (compute_state_change_act currentVertex) in
													(Plan.add plan stateChangeAct);
													(print_endline ("It's an intermediate vertex => add action " 
															^ (Action.to_string stateChangeAct) ^ " to the plan."));
													(Printf.bprintf !file_buffer "%s\n" ("It's an intermediate vertex => add action " 
															^ (Action.to_string stateChangeAct) ^ " to the plan."))
												end;
											(* deal with instance successor *)
											(print_endline "Deal with successor vertex.");
											(Printf.bprintf !file_buffer "%s\n" "Deal with successor vertex.");
											(process_inst_edge plan toVisit file_buffer currentVertex)
										end;
										(* if we reach the target node *)
										if (currentVertex.comp_type_name = targetComponent) && 
												((extract_tag_dst_name currentVertex.tag) = targetState) then begin 
													(print_endline "Target has been REACHED.");
													(Printf.bprintf !file_buffer "%s\n" "Target has been REACHED.");
													finished := true
											end;
										(* delete current vertex from vertices list *)
										vertices := (remove_from_list currentVertex !vertices); 
										(print_endline ("Vertex removed: " ^ (to_string_with_id currentVertex))); 
										(Printf.bprintf !file_buffer "%s\n" ("Vertex removed: " ^ (to_string_with_id currentVertex))); 
										j
          				end)
								(* Inner loop condition: stop when we reach a fixpoint (no new nodes are added) or we find target *)
								(fun j -> ((Stack.is_empty toVisit) || !finished)) 
      					~init:(ref 0));
							(* If we finished without reaching target => there are unvisited 
									vertices (couldn't be visited in topological order) => need duplication *)
							if !finished = false then begin
								(print_endline "\n ************************* NEED INSTANCE DUPLICATION *************************** "); 
								(Printf.bprintf !file_buffer "%s\n" "\n ************************* NEED INSTANCE DUPLICATION *************************** "); 
								let vertex = (duplicate vertices plan toVisit file_buffer) in begin
									(print_endline ("DUPLICATED Vertex: " ^ (to_string_full vertex)));
									if vertex.nr_in_edges = 0 then begin
										(Stack.push vertex toVisit);
										(Printf.bprintf !file_buffer "%s\n" ("Pushed duplicated vertex: " ^ (to_string_with_id vertex)));
										(print_endline (">>>>>>>>>>>>>>> PUSH duplicated vertex: " ^ (to_string_with_id vertex)));
										(print_stack toVisit file_buffer) 
									end; 
								end;
							end;	
							i
          	end)
					(* External loop condition *)
					(fun i -> !finished)
      	~init:(ref 0));
				plan

			
				
      (**************************************************)
      (*                  Topological sort              *)  
      (**************************************************)
      
			let top_sort vertices =
        let sorted_vertices = (ref []) in
        let start_vertices = (List.filter has_no_in_edges vertices) in
        (* at the beginning we only have initial vertices *)
        let work_list = (ref start_vertices) in
        while (not_empty !work_list) do
          begin
            let current_vertex = (extract_from work_list) in
            (* associate actions to vertex *)
            let computed_actions = (compute_actions current_vertex) in
            (set_actions current_vertex computed_actions);
            (* add to working_list the successors that have no incoming edge as
              a side effect of removing edges from vertex *)
            let successors_refs = (get_successors current_vertex) in
						let successors = (elim_duplicates (List.map (fun vertex_ref -> !vertex_ref) successors_refs)) in
              begin
                (remove_edges current_vertex);
                let no_in_edge_succs = (List.filter has_no_in_edges successors) in
                (add_vertices work_list no_in_edge_succs);
              end;
            sorted_vertices := current_vertex :: !sorted_vertices;
          end;
        done;
        !sorted_vertices 

      (*  Topological sort used for debugging many prints to see what happens *)  
      let top_sort_DEBUG vertices =
        let sorted_vertices = (ref []) in
        let start_vertices = (List.filter has_no_in_edges vertices) in
        (* at the beginning we only have initial vertices *)
        let work_list = (ref start_vertices) in
        let i = (ref (2*(List.length vertices))) in
        while ((not_empty !work_list) && (!i > 0)) do
          begin
            let vertex = (extract_from work_list) in
            let vertex_string = (to_string_with_id vertex) in
            print_endline ("\nvertex extracted: " ^ vertex_string);
            (* associate actions to vertex *)
            let actions = (compute_actions vertex) in
            (set_actions vertex actions);
            let actions_string = (Action.string_of_actions_list actions) in
            print_endline ("the actions computed are: " ^ actions_string);
            (* add to working_list the successors that have no incoming edge as
              a side effect of removing edges from vertex *)
            let successors_refs = (get_successors vertex) in
						let successors = (elim_duplicates (List.map (fun vertex_ref -> !vertex_ref) successors_refs)) in
            let succs_string = (ref "{ }") in
            if (successors != []) then
              succs_string := (to_string_with_id_list successors);
            print_endline ("the whole list of its successors is: " ^ !succs_string);
              begin
                (remove_edges vertex);
                let no_in_edge_succs = (List.filter has_no_in_edges successors) in
                (add_vertices work_list no_in_edge_succs);
                let succs_no_in_edge_str = (ref "{ }") in
                if (no_in_edge_succs != []) then
                        succs_no_in_edge_str := (to_string_with_id_list no_in_edge_succs);
                print_endline ("we add to working set the following vertices with no incoming"
                ^ " edge: " ^ !succs_no_in_edge_str);
              end;
            sorted_vertices := vertex :: !sorted_vertices;
            let sorted_vertices_str = (to_string_with_id_list (List.rev !sorted_vertices)) in
            print_endline ("the sorted vertices up to now are: " ^
            sorted_vertices_str);
            i := (!i - 1)
          end;
        done;
        !sorted_vertices 


    end



(****************************************************************)
(*			                Instance Edge type                 			*)
(*        edges between vertices on the same instance line      *)
(****************************************************************)

    and Inst_edge : sig
    	type t
      val make : (Gg.Node.t ref) -> Vertex.t -> t
      val to_string : t -> string
      val to_string_opt : t option -> string
      val get_comp_type : t -> (component_t ref)
      val get_state : t -> (state_t ref)       
      val get_dest : t -> (Vertex.t ref)       
      val set_dest : t -> (Vertex.t ref) -> unit       
      val get_tag : t -> (Gg.Node.t ref)            
      val get_comp_type_from_tag : (Gg.Node.t ref) -> (component_t ref)        
      val get_state_from_tag : (Gg.Node.t ref) -> (state_t ref)
			val extract_value : t option -> t       
    end = struct

			exception No_inst_edge

      type t = {
      	mutable dest : Vertex.t ref; 
        tag : Gg.Node.t ref 
      }

      (* this function simply extracts a value of type Inst_edge.t from an option *)    
      let extract_value inst_edge_opt =
        match inst_edge_opt with
        None -> raise No_inst_edge
        | (Some edge) -> edge
      
      let get_dest edge =
        edge.dest
      
      let get_tag inst_edge =
        inst_edge.tag
			
			let set_dest edge new_dest =
        edge.dest <- new_dest

      let get_comp_type_from_tag tag =  
        let comp_type = (Gg.Node.get_res_type !tag) in
				comp_type 
      
      let get_state_from_tag tag =  
        let state = (Gg.Node.get_state !tag) in
        state 

      let to_string_tag tag =
        (Gg.Node.to_string !tag)

      let to_string edge =
        let tag_str = (to_string_tag edge.tag) in
        let string_repr = "--" ^ tag_str ^ "-->" in
        string_repr 
      
			let to_string_full edge =
        let tag_str = (to_string_tag edge.tag) in
        let string_repr = "--" ^ tag_str ^ "-->" in
				let dest_vertex = !(edge.dest) in
				let dest_str = (Vertex.to_string_with_id dest_vertex) in
        let full_repr = (string_repr ^ " " ^ dest_str) in
				full_repr

			let to_string_opt edge_option =
				match edge_option with
					None -> "{ }"
				| (Some edge) -> (to_string_full edge)      

      let make gnode_ref vertex =
        let new_inst_edge = {
          dest = (ref vertex);
          tag = gnode_ref
        } in
        new_inst_edge 
      
			(* extract the component type that tags an instance edge *)
      let get_comp_type inst_edge =
        let comp_type = (get_comp_type_from_tag inst_edge.tag) in
        comp_type      
      
      (* extract the state that tags an instance edge *)
      let get_state inst_edge =
        let a_state = (get_state_from_tag inst_edge.tag) in
        a_state    

    end


(********************************************************************)
(*			                Dependency Edge type                   			*)
(* connecting vertices of different instances (blue and red edges)  *)
(********************************************************************)

    and Dep_edge : sig
      
      (* go edges are blue while return edges are red *)    
      type color_t 
      type t
      val to_string : t -> string
      val string_of_list : t list -> string
      val get_dest : t -> (Vertex.t ref)       
      val set_dest : t -> (Vertex.t ref) -> unit       
      val extract_dest_vrtx : t -> Vertex.t       
      val get_port : t -> string
      val get_dest_id : t -> string
      val make_go : (Vertex.t ref) -> port_name -> t 
      val make_return : (Vertex.t ref) -> port_name -> t
			val set_twin : t -> t -> unit
			val set_mutual_twins : t -> t -> unit
			val get_twin : t -> t ref
			val remove_edge : t -> (t list) -> (t list) 
			val duplicate_red_edges : Vertex.t -> (t list) -> Buffer.t ref -> (t list)
			val duplicate_blue_edges : Vertex.t ref -> (t * Vertex.t) list -> Buffer.t ref -> (t list)
			val duplicate_blue_red_edges : Vertex.t ref -> (t * Vertex.t) list -> Buffer.t ref -> unit
			val fix_twins_list : (t list) -> (t list) -> unit
    end = struct

			exception Twin_not_set of string
			exception Different_nr_of_edges of string
			exception Many_twin_blue_edges of string
      
			(* go edges are blue while return edges are red *)    
      type color_t = Blue | Red

      let string_of_color color =
        match color with
          Blue -> "GO"
        | Red -> "RETURN" 
            
      type t = {
        mutable dest : (Vertex.t ref); 
        mutable twin : (t ref) option;
        port : port_name;
        color : color_t 
      }

			(* N.B. for the moment we left out the "twin" field *)
			let eq e1 e2 =
				(!(e1.dest) == !(e2.dest)) 
				&& (e1.port = e2.port)
				&& (e1.color = e2.color)

			let set_twin edge twin_edge =
				edge.twin <- (Some (ref twin_edge))

      let get_dest edge =
        edge.dest
        
      let get_port edge =
        edge.port

      let set_dest edge new_dest =
        edge.dest <- new_dest

			let extract_dest_vrtx edge =
				!(edge.dest)

      let get_dest_id edge =
        let dst_vertex = !(edge.dest) in
        let dst_vertex_id = (Vertex.get_id dst_vertex) in
        dst_vertex_id 

      let to_string edge =
        let dest_str = (Vertex.to_string_with_id !(edge.dest)) in
        let string_repr = ("---" 
        ^ edge.port ^ "---> " ^ dest_str) in
        string_repr

			let string_of_list edges_list = 
        let string_list = (List.map to_string edges_list) in
				let string_repr = (String.concat "; " string_list) in
				string_repr

      let make_go dest_vertex a_port =
        let new_go_edge = {
          dest = dest_vertex;
          twin = None;
          port = a_port;
          color = Blue
        } in
        new_go_edge 
      
      let make_return dest_vertex a_port =
        let new_return_edge = {
          dest = dest_vertex;
          twin = None;
          port = a_port;
          color = Red
        } in
        new_return_edge

			let extract_twin edge =
				match edge.twin with
					None -> raise (Twin_not_set ("in edge " ^ (to_string edge)))
				|	(Some edge_ref) -> edge_ref

			let get_twin = extract_twin

			let rec remove_edge edge edge_list =
				match edge_list with
					[] -> []
				|	head :: tail ->
						begin
							if (eq head edge) then
								(remove_edge edge tail)
							else
								head :: (remove_edge edge tail)
						end
			
			let set_mutual_twins e1 e2 =
				(set_twin e1 e2);
				(set_twin e2 e1)

	let duplicate_red_edges src_vertex edges file_buffer =
		let duplicate_single_red_edge src edge =
			let dest = (get_dest edge) in
			let port = (get_port edge) in
			let new_edge = (make_return dest port) in
			(Vertex.add_return_edge src_vertex new_edge);
			(Printf.bprintf !file_buffer "%s\n" ("added the following RED/RETURN edge: "
				^ (Vertex.to_string_with_id src_vertex) ^ " " ^ (to_string new_edge))); 
			new_edge
		in
		(List.map (duplicate_single_red_edge src_vertex) edges) 	 

	let duplicate_blue_edges dst_vertex edges_vertices_pairs file_buffer =
		let duplicate_single_blue_edge dst edge_vertex_pair =
			let edge = (fst edge_vertex_pair) in			
			let port = (get_port edge) in
			let src_vertex = (snd edge_vertex_pair) in			
			let new_edge = (make_go dst port) in
			(Vertex.add_go_edge src_vertex new_edge);
			(Printf.bprintf !file_buffer "%s\n" ("added the following BLUE/GO edge: "
				^ (Vertex.to_string_with_id src_vertex) ^ " " ^ (to_string new_edge))); 
			new_edge
		in
		(List.map (duplicate_single_blue_edge dst_vertex) edges_vertices_pairs) 	 


	let duplicate_blue_red_edges new_dest edges_vertices_pairs file_buffer =
		let duplicate_edges_pair dst edge_vertex_pair =
			let orig_blue_edge = (fst edge_vertex_pair) in			
			let port = (get_port orig_blue_edge) in
			let orig_src_vertex = (snd edge_vertex_pair) in
			let new_blue_edge = (make_go new_dest port) in
			(Vertex.add_go_edge orig_src_vertex new_blue_edge);
			(Printf.bprintf !file_buffer "%s\n" ("added the following BLUE/GO edge: "
				^ (Vertex.to_string_with_id orig_src_vertex) ^ " " ^ (to_string new_blue_edge)));
			let orig_red_edge = !(get_twin orig_blue_edge) in
			let red_edge_dest = (get_dest orig_red_edge) in
			let new_red_edge = (make_return red_edge_dest port) in
			(set_mutual_twins new_blue_edge	new_red_edge);				 
			if (Vertex.is_not_final !new_dest) then
				begin
					let new_red_edge_src = (Vertex.get_succ !new_dest) in
					(Vertex.add_return_edge new_red_edge_src new_red_edge);
					(Printf.bprintf !file_buffer "%s\n" ("added the following RED/RETURN edge: "
						^ (Vertex.to_string_with_id new_red_edge_src) ^ " " ^ (to_string new_red_edge)));
				end
			else
				begin
					let new_red_edge_src = !new_dest in
					(Vertex.add_return_edge new_red_edge_src new_red_edge);
					(Printf.bprintf !file_buffer "%s\n" ("added the following RED/RETURN edge: "
						^ (Vertex.to_string_with_id new_red_edge_src) ^ " " ^ (to_string new_red_edge)));
				end
		in
		(List.iter (duplicate_edges_pair new_dest) edges_vertices_pairs) 	 


	let fix_twins_list red_edges blue_edges =
		let nr_red_edges = (List.length red_edges)
		and nr_blue_edges = (List.length blue_edges) in
		if nr_red_edges != nr_blue_edges then
			raise (Different_nr_of_edges ("There are " ^ (string_of_int nr_red_edges)
				^ " red edges and " ^ (string_of_int nr_blue_edges) ^ " blue edges."))
		else
			begin
				let red_edges_array = (Array.of_list red_edges) in
				for i = 0 to (nr_red_edges - 1)  do
					let current_red_edge = red_edges_array.(i) in
					let current_port = (get_port current_red_edge) in
					let twin_edges = (List.filter 
															(fun edge -> (get_port edge) = current_port)
															blue_edges) in
					let nr_twin_edges = (List.length twin_edges) in
					if nr_twin_edges > 1 then
						raise (Many_twin_blue_edges ("There is more than one twin blue edge"
							^ " for red edge: " ^ (to_string current_red_edge)))
					else
						begin
							let twin_blue_edge = (List.hd twin_edges) in
							(set_mutual_twins current_red_edge twin_blue_edge)
						end;
				done	
 			end

end

  end
