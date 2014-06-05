
open My_datatypes
open Datatypes_t
open Facade
open Gg
open T

  
(********************************************************************)
(*			            Module for instance lines                 			*)
(********************************************************************)

	exception Empty_vertices
	exception No_pairs
	exception Go_edge_not_found of string
	exception No_corresponding_vertex of string
	exception Instance_not_found of string
	exception Dest_vertices_not_on_same_instance of string
	exception Vertex_not_found of string

  type t = {
    comp_type : component_t ref;     
		mutable duplicates_nr : int;
    id : string;
    mutable vertices : T.Vertex.t list;
  }

	let get_id instance = instance.id

	let length instance =
		(List.length instance.vertices)

	(* special kind of equality used in the splitting process 
	 * it checks that the two instances:
	 *   - are of the same component type
	 *   - have equal vertices (for this we only look at their tag, like (s,s') )
	 *)
	let eq i1 i2 =
		try
			let comp_eq = ( !(i1.comp_type) == !(i2.comp_type) ) in
			let verts_eq = (List.fold_left2 (fun a v1 v2 -> a && (T.Vertex.eq_tag v1 v2)) true i1.vertices i2.vertices) in
			let result = comp_eq && verts_eq in
			result
		with (Invalid_argument str) -> false

	(* function involved in the splitting phase *)
	(* find the instance line that has the same id as the given vertex *)
	let find_by_vertex vertex instances =
		let v_id = (T.Vertex.get_id vertex) in
		let match_instance_id vertex_id instance = (vertex_id = instance.id) in
		(List.find (match_instance_id v_id) instances) 

	let compute_duplicate_id original_id duplicates_nr =
		let duplicate_id = (ref original_id) in
		for i = 1 to duplicates_nr do
			duplicate_id := !duplicate_id ^ "'" 
		done;
		!duplicate_id 

	(* TODO: duplicate instance name generation is quite rudimentary: 
		 if we copy twice x -> x''
		 if we copy a copy of x we obtain the same name x''
	*)
	let copy_line_until_vertex iline vertex = 
		let new_vertices = (ref []) in
		let new_duplicates_nr = (iline.duplicates_nr + 1) in
		let copy_instance = {
			comp_type = iline.comp_type;
			duplicates_nr = new_duplicates_nr; 
			(*id = iline.id ^ "'";*)
			id = (compute_duplicate_id iline.id new_duplicates_nr); 
			vertices = (T.Vertex.copy_vertices_until new_vertices vertex iline.vertices);
		} in
		copy_instance
  
  let get_comp_type instance =
    instance.comp_type
    
  let get_vertices instance =
    instance.vertices
  
  let set_vertices instance new_vertices =
    instance.vertices <- new_vertices

  let rev_vertices instance =
    instance.vertices <- (List.rev instance.vertices)        

  let to_string instance =
    let string_repr = "Instance " ^ instance.id ^ " : " 
      ^ (T.Vertex.to_string_list instance.vertices) in
    string_repr

  let to_string_full instance =
    let string_repr = "Instance " ^ instance.id ^ " :\n" 
      ^ (T.Vertex.to_string_list_full instance.vertices) in
    string_repr
  
  let print instance =
    print_endline (to_string instance)  

  let to_string_list instances_list =
    let string_list = (List.map to_string instances_list) in
    let string_repr = (String.concat "\n\n" string_list) in
    string_repr  
  
	let to_string_list_full instances_list =
    let string_list = (List.map to_string_full instances_list) in
    let string_repr = (String.concat "\n\n" string_list) in
    string_repr  

  let print_list file_buffer instances_list =
		let string_repr = (to_string_list_full instances_list) in
		(Buffer.add_string !file_buffer (string_repr ^ "\n"))


	(************************************************************)
	(*						dealing with DOT file representation					*)
	(************************************************************)
  
	let dotStringFormat = Str.global_replace (Str.regexp "-") "_"
  
	let dot_of_nodes instance =
		let instance_comment = ("\n\t// Instance line of " ^ instance.id) in
		let cluster_id = ("cluster_" ^ (dotStringFormat instance.id)) in
		let header = "\n\tsubgraph " ^ (dotStringFormat cluster_id) ^ " {" in
		(*
    let style_str = "\n\tstyle=filled;" in
    let style_str = "\n\tnode[style=filled];" in
    let style_str = "" in
    let color_str = "\n\t\tcolor=blue;" in
		*)
    let style_str = "\n\t\tnode[style=filled, color=lightgrey];" in
    let color_str = "" in
    let label_str = "\n\t\tlabel=\"" ^ (String.uppercase instance.id) ^ "\";" in
		let closing = "\n\t}" in
    let nodes_repr = (T.Vertex.dot_of_nodes_list instance.vertices) in
		let string_repr = (instance_comment ^ header ^ style_str ^ color_str 
			^ nodes_repr ^ label_str ^ closing) in
		string_repr 
  
	let dot_of_nodes_list instances_list =
    let string_list = (List.map dot_of_nodes instances_list) in
    let string_repr = (String.concat "\n" string_list) in
    string_repr  
	
	let dot_of_edges instance =
		let instance_comment = (ref "") in
    let edges_repr = (T.Vertex.dot_of_edges_list instance.vertices) in
		if edges_repr <> "" then
			instance_comment := ("\n\t// Edges from instance line of " ^ instance.id); 
		let string_repr = !instance_comment ^ edges_repr in
		string_repr 

	let dot_of_edges_list instances_list =
    let string_list = (List.map dot_of_edges instances_list) in
    let string_repr = (String.concat "\n" string_list) in
    string_repr  

  let print_abstract_plan file_buffer instances_list =
		let pre_header = "// Abstract plan in DOT file representation \n\n" in
		let nodes_string_repr = (dot_of_nodes_list instances_list) in
		let dep_edges_string_repr = (dot_of_edges_list instances_list) in
		let header = "digraph {\n" in
		let closing = "\n}" in
		(Buffer.add_string !file_buffer pre_header);
		(Buffer.add_string !file_buffer header);
		(Buffer.add_string !file_buffer nodes_string_repr);
		(Buffer.add_string !file_buffer dep_edges_string_repr);
		(Buffer.add_string !file_buffer closing)
		
  let make component_type inst_id  =
    let new_instance = { 
      comp_type = component_type; 
			duplicates_nr = 0;
      id = inst_id; 
      vertices = [] 
    } in
    new_instance

  let add_vertex instance vertex =
    instance.vertices <- vertex :: instance.vertices     

  (* this function transforms a list of nodes into an instance line *)
  let rec build_instance_line instance_line last_added_vertex node_list comp_type =
    match node_list with
      [] -> instance_line
    | [head] ->
        begin
          let inst_id = instance_line.id in      
          let dupl_nr = instance_line.duplicates_nr in      
          let head_state = (Gg.Node.get_state head) in      
          let head_state_id = (get_state_id !head_state) in
          let delete_vertex = (T.Vertex.make_delete head_state_id inst_id comp_type dupl_nr) in
          (* abnormal case of an initial node without any evolution *)      
          if (Gg.Node.is_initial head) then begin
            let create_vertex = (T.Vertex.make_create head_state_id inst_id comp_type dupl_nr) in
            (T.Vertex.set_inst_edge create_vertex delete_vertex (ref head));
            (add_vertex instance_line create_vertex)
          (* must be final node *)  
          end else begin
            let last_vertex = (T.Vertex.extract_vertex last_added_vertex) in      
            (T.Vertex.set_inst_edge last_vertex delete_vertex (ref head));
          end;
          (add_vertex instance_line delete_vertex);
        (rev_vertices instance_line)   
        end;
        instance_line
    | head :: tail ->
        begin
          let inst_id = instance_line.id in      
          let dupl_nr = instance_line.duplicates_nr in      
          let head_of_tail = (List.hd tail) in       
          let head_state = (Gg.Node.get_state head) in       
          let head_state_id = (get_state_id !head_state) in       
          let head_of_tail_state = (Gg.Node.get_state head_of_tail) in       
          let head_of_tail_state_id = (get_state_id !head_of_tail_state) in       
          let new_vertex = (T.Vertex.make head_state_id head_of_tail_state_id inst_id comp_type dupl_nr) in
          (* initial node case *)
          if (Gg.Node.is_initial head) then begin
            let create_vertex = (T.Vertex.make_create head_state_id inst_id comp_type dupl_nr) in
            (T.Vertex.set_inst_edge create_vertex new_vertex (ref head));
            (add_vertex instance_line create_vertex)
          (* standard case inner nodes *)
          end else begin 
            let last_vertex = (T.Vertex.extract_vertex last_added_vertex) in      
            (T.Vertex.set_inst_edge last_vertex new_vertex (ref head))
          end;
          (add_vertex instance_line new_vertex);
          last_added_vertex := (Some new_vertex);
          (build_instance_line instance_line last_added_vertex tail comp_type)
        end        
  
	(* just checks if a comp type is in the given list *)	
  let rec in_list ct ct_list =
	  match ct_list with
		  [] -> false
	  |	head :: tail -> (ct.cname = head.cname) || (in_list ct tail) 
	
	let rec elim_duplicates ct_list =
				match ct_list with
					[] -> []
				| head :: tail -> 
						begin
							if (in_list head tail) then
								(elim_duplicates tail)
							else 
								head :: (elim_duplicates tail)
						end

	let get_path_comp_type path =
		let head_node = (List.hd path) in 
		let comp_type = (Gg.Node.get_res_type head_node) in
		!comp_type

	let compute_ct_list paths =
		let original_ct_list = (List.map get_path_comp_type paths) in
		let unique_ct_list = (elim_duplicates original_ct_list) in
		unique_ct_list 

	let find_max paths =
		let comparison path1 path2 =
			if ((List.length path1) < (List.length path2)) then
				1
			else if ((List.length path1) > (List.length path2)) then     
				(-1)
			else 
				0
		in
		let sorted_paths = (List.sort comparison paths) in
		let maximal_path = (List.hd sorted_paths) in
		maximal_path

	let eq_comp_type ct1 ct2 =
		(ct1.cname = ct2.cname)

	let keep_max comp_type paths =
		let filtered_paths = 
			(List.filter
			(fun path -> (eq_comp_type (get_path_comp_type path) comp_type))
			paths) in
		let max_path = (find_max filtered_paths) in
		max_path	 
	
	let rec keep_max_paths ct_list paths =
		match ct_list with
			[] -> []
		|	head :: tail -> (keep_max head paths) :: (keep_max_paths tail paths) 
		
	let filter_maximal_paths paths =
		let ct_list = (compute_ct_list paths) in
		let maximal_paths = (keep_max_paths ct_list paths) in
		maximal_paths 
		
	(* here we use iteration instead of recursion to add an index for unique instance line (and hence vertex) ID *)
  let build_instance_lines list_of_nlist =
    let aux_fun nlist =
      let inst_id = (Gg.Node.make_id nlist) in
      let comp_type = (Gg.Node.extract_comp_type nlist) in
      let inst_line = (make comp_type inst_id) in
      let last_added_vertex = (ref None) in
      (build_instance_line inst_line last_added_vertex nlist comp_type)
    in
    let instance_lines = (ref []) in
    let length = (List.length list_of_nlist) in
    for i = 0 to (length - 1) do
      let current_nlist = (List.nth list_of_nlist i) in      
      let new_instance_line = (aux_fun current_nlist) in
      instance_lines := new_instance_line :: !instance_lines
    done;
    !instance_lines

  let filter_by_comp_type comp_type inst_list =
    let eq_comp comp_t instance =
      let inst_comp = instance.comp_type in
      let inst_comp_name = (!inst_comp).cname in
      let comp_type_name = (!comp_type).cname in
      (comp_type_name = inst_comp_name)
    in      
    let filtered_list = (List.filter (eq_comp comp_type) inst_list) in
    filtered_list
  
	(* search many instance lines for a vertex tagged with the right arrival state *) 
  let rec find_vertex_by_state a_state instance_lines =
    let find_in_single_inst_line astate inst_line =
      (T.Vertex.find_in_list_by_state a_state inst_line.vertices)
    in
    match instance_lines with
      [] -> raise Not_found
    | head_inst_line :: other_lines ->
        begin
          try 
            let vertex = (find_in_single_inst_line a_state head_inst_line) in
            vertex
          with
            Not_found -> (find_vertex_by_state a_state other_lines) 
        end

   (* it scans the instance line of "vertex" looking for the farthest vertex
    * providing the same "port" *) 
   let rec find_latest_provider vertex port =
    if (T.Vertex.has_successor vertex) then
      begin         
        let inst_edge = (T.Vertex.get_inst_edge vertex) in
				let state = !(T.Inst_edge.get_state inst_edge) in
        if (List.mem port state.provides) then
          begin        
            let next_vertex = (T.Inst_edge.get_dest inst_edge) in
            (find_latest_provider !next_vertex port)
          end
        else
          vertex  
      end
    else
         vertex   
		
		(* add to vertex a pair of go and return edges corresponding to one binding
   	* (represented by a port-node pair) *) 
   	let add_go_return_edge_DEBUG instance_lines vertex bound_port_node_pair =
    	let bound_port = (fst bound_port_node_pair) in      
    	let bound_node = !(snd bound_port_node_pair) in      
    	let bound_comp_type = (Gg.Node.get_res_type bound_node) in 
    	let bound_node_state = (Gg.Node.get_state bound_node) in 
    	let bound_node_state_id = (get_state_id !bound_node_state) in 
    	(* restrict search to instance lines of the right component type (that of bound node) *)    
    	let filtered_instances = (filter_by_comp_type bound_comp_type instance_lines) in
    	(* add go (blue) edge *)
    	let go_src_vertex = (find_vertex_by_state bound_node_state_id filtered_instances) in
    	let go_edge = (T.Dep_edge.make_go (ref vertex) bound_port) in
    	(T.Vertex.add_go_edge go_src_vertex go_edge);
    	let go_src_str = (T.Vertex.to_string_with_id go_src_vertex) in
    	let go_edge_str = (T.Dep_edge.to_string go_edge) in
    	print_endline ("\nadded the following GO edge: " ^ go_src_str ^ " " ^ go_edge_str);
    	(* add return (red) edge *)
    	let inst_edge = (T.Vertex.get_inst_edge vertex) in
    	let return_src_vertex = (T.Inst_edge.get_dest inst_edge) in
    	(* find a provider for the same port as forward as possible in the same
     	* instance line (widen the return edge) *) 
    	let return_inst_edge = (T.Vertex.get_inst_edge go_src_vertex) in
    	let start_return_dst_vertex = (T.Inst_edge.get_dest return_inst_edge) in
    	let return_dst_vertex = (find_latest_provider !start_return_dst_vertex bound_port) in
    	let return_edge = (T.Dep_edge.make_return (ref return_dst_vertex) bound_port) in
    	(T.Vertex.add_return_edge !return_src_vertex return_edge);
    	let return_src_str = (T.Vertex.to_string_with_id !return_src_vertex) in
    	let return_edge_str = (T.Dep_edge.to_string return_edge) in
    	print_endline ("added the following RETURN edge: " ^ return_src_str ^ " " ^ return_edge_str);
			(* each of the two edges refernces the other, its twin *)
			(T.Dep_edge.set_twin go_edge return_edge);
			(T.Dep_edge.set_twin return_edge go_edge)
		
  	(* add to vertex a pair of go and return edges corresponding to one binding
   	* (represented by a port-node pair) *) 
   	let add_go_return_edge instance_lines vertex bound_port_node_pair =
    	let bound_port = (fst bound_port_node_pair) in      
    	let bound_node = !(snd bound_port_node_pair) in      
    	let bound_comp_type = (Gg.Node.get_res_type bound_node) in 
    	let bound_node_state = (Gg.Node.get_state bound_node) in 
    	let bound_node_state_id = (get_state_id !bound_node_state) in 
    	(* restrict search to instance lines of the right component type (that of bound node) *)    
    	let filtered_instances = (filter_by_comp_type bound_comp_type instance_lines) in
    	(* add go (blue) edge *)
    	let go_src_vertex = (find_vertex_by_state bound_node_state_id filtered_instances) in
    	let go_edge = (T.Dep_edge.make_go (ref vertex) bound_port) in
    	(T.Vertex.add_go_edge go_src_vertex go_edge);
    	(* add return (red) edge *)
    	let inst_edge = (T.Vertex.get_inst_edge vertex) in
    	let return_src_vertex = (T.Inst_edge.get_dest inst_edge) in
    	(* find a provider for the same port as forward as possible in the same
     	* instance line (widen the return edge) *) 
    	let return_inst_edge = (T.Vertex.get_inst_edge go_src_vertex) in
    	let start_return_dst_vertex = (T.Inst_edge.get_dest return_inst_edge) in
    	let return_dst_vertex = (find_latest_provider !start_return_dst_vertex bound_port) in
    	let return_edge = (T.Dep_edge.make_return (ref return_dst_vertex) bound_port) in
    	(T.Vertex.add_return_edge !return_src_vertex return_edge);
			(* each of the two edges references the other, its twin *)
			(T.Dep_edge.set_twin go_edge return_edge);
			(T.Dep_edge.set_twin return_edge go_edge)

  (* add go and return edges to vertex *) 
  let vertex_add_dep_edges instance_lines vertex =
    if (T.Vertex.has_successor vertex) then
      begin         
        let inst_edge = (T.Vertex.get_inst_edge vertex) in
        let edge_tag = (T.Inst_edge.get_tag inst_edge) in
        let node = !edge_tag in
        let bound_port_node_pairs = (Gg.Node.get_bound_ports_nodes node) in
				(*
				let string_repr = (Gg.Node.to_string_with_bindings node) in
				(print_endline string_repr);
				*)
				(*
        (List.iter (add_go_return_edge_DEBUG instance_lines vertex) bound_port_node_pairs) 
				*)
        (List.iter (add_go_return_edge instance_lines vertex) bound_port_node_pairs) 
      end   

  (* this function adds go (blue) and return (red) edges to a single instance *)
  let instance_add_dep_edges instance_lines instance =
    (List.iter (vertex_add_dep_edges instance_lines) instance.vertices)        

  (** this function adds go (blue) and return (red) edges.
  		[inst_lines] is a list of instances. *)    
  let list_add_dep_edges inst_lines =
    (List.iter (instance_add_dep_edges inst_lines) inst_lines) 


	(* given a list of vertices it builds the pair (head, tail) *)
	let to_head_tail_pair instance_line =
		match instance_line.vertices with
			[] -> raise Empty_vertices
		|	head :: tail -> (head, tail)
  
	(* instance_lines is a list of instances *)  
  (* given a list of instances we extract a single list of vertices *)  
  let list_to_vertices instance_lines =
		let head_tail_pairs = (List.map to_head_tail_pair instance_lines) in
		let vertices_lists_pair = (List.split head_tail_pairs) in
		let initial_vertices_list = (fst vertices_lists_pair) in
		let rest_vertices_list = (List.flatten (snd vertices_lists_pair)) in
    let merged_vertices =  initial_vertices_list @ rest_vertices_list in
    merged_vertices    

  (* instance_lines is a list of instances *)  
  (* given a list of instances we extract a single list of vertices *)  
  let old_list_to_vertices instance_lines = 
    let vertices_lists = (List.map get_vertices instance_lines) in
    let merged_vertices = (List.flatten vertices_lists) in
    merged_vertices    

	
	let find_src_by_go_edge instance go_edge =
		try
			let src = (T.Vertex.find_in_list_by_go_edge go_edge instance.vertices) in
			src
		with Not_found -> raise (Go_edge_not_found ("Unbale to find a vertex that" 
			^ " has the given go edge " ^ (T.Dep_edge.to_string go_edge) ^ " in the "
			^"following instance line: " ^ (to_string instance))) 
		 

	let get_final_vertex instance =
		let length = (List.length instance.vertices) in
		let final_vertex = (List.nth instance.vertices (length-1)) in
		final_vertex

	(* this function tests to see if the given instance line has 
	 * as a final vertex (s,D) where (s,s') is the given vertex
	 * i.e. that chosen for splitting 
	 *)
	let is_usable_iline iline vertex =
		let length = (List.length iline.vertices) in
		let final_vertex = (List.nth iline.vertices (length-1)) in
		let final_tag_src = (T.Vertex.extract_src_from_tag final_vertex) in
		let vertex_tag_src = (T.Vertex.extract_src_from_tag vertex) in
		if (final_tag_src = vertex_tag_src) then
			true
		else
			false

	let find_in_edges_vertices_pairs vertex instances =
		let rec find_in_edges_aux vertex instances =
			match instances with
				[] -> []
			|	head :: tail ->
					begin
						let edges = (T.Vertex.find_in_blue_edges_vertices vertex head.vertices) in
						edges :: (find_in_edges_aux vertex tail)
					end
		in
		(List.concat (find_in_edges_aux vertex instances))

	let find_corresponding_vertex vertex splitted_inst duplicate_inst =
		let nth_vertex pos instance =
			try
				(List.nth instance.vertices pos)
			with (Failure msg) -> raise (No_corresponding_vertex ("vertex " 
				^ (T.Vertex.to_string_with_id vertex) ^ " from instance line " 
				^ (to_string splitted_inst) ^ " has no corresponding vertex in "
				^ (to_string duplicate_inst)))
		in
		let position = (ref 0) in
		let vertex_position = (T.Vertex.find_position position vertex splitted_inst.vertices) in
		let corresponding_vertex = (nth_vertex vertex_position duplicate_inst) in
		corresponding_vertex 

	let rec find_by_id inst_id instances =
		match instances with
			[] -> raise (Instance_not_found ("instance " ^ inst_id 
							^ " could not be found among: " ^ (to_string_list instances))) 
		|	head :: tail ->
				begin
					if (head.id = inst_id) then
						head
					else
						(find_by_id inst_id tail) 	
				end 

	(* search many instance lines for one that stops in the needed state *) 
	(* TODO must implement get_vertex_comp_type
  let find_ready_iline vertex ilines =
  	let rec find_ready_iline_aux vertex instances =
    	match instances with
      	[] -> raise Not_found
      	| hd_iline :: other_lines ->
        		begin
							if (is_usable_iline hd_iline vertex) then
								hd_iline
							else 
            		(find_ready_iline_aux vertex other_lines)
        		end
		in
		let comp_type = (T.Vertex.get_comp_type vertex) in
		let filtered_ilines = (filter_by_comp_type comp_type ilines) in
		(find_ready_iline_aux vertex filtered_instances)
	*)



	(****************************************************)
	(*								FIX OVERLAPPING EDGES							*)
	(****************************************************)

	(* Simply check if [edge] is tagged with same name as [port]. *)
	let same_port port edge =
		let edge_port = (T.Dep_edge.get_port edge) in
		(port = edge_port)
		
	(* Simply check if destination vertex of [edge] is on the same [instance]. *)
	let same_instance instance edge =
		let edge_dst = !(T.Dep_edge.get_dest edge) in
		let dst_instance = (T.Vertex.get_id edge_dst) in
		(instance = dst_instance)

	(** Compute the position of [vertex] in the instance line [instance] starting 
			from the initial one [pos]. *)
	let compute_vertex_position pos instance vertex =
		let rec compute_vertex_position_aux pos vertices vertex =
			match vertices with
				[] -> raise (Vertex_not_found ("vertex " ^ (T.Vertex.to_string vertex) 
								^ " could not be found on instance " ^ (T.Vertex.get_id vertex)))  
			| head :: tail -> begin
					if (T.Vertex.eq_id_tag head vertex) then
						!pos
					else begin
						pos := !pos + 1;
						(compute_vertex_position_aux pos tail vertex)
					end		
				end
		in
		(compute_vertex_position_aux pos instance.vertices vertex)

	(** Check if [vertex] is farther down on [instance] w.r.t. destination vertex 
		of [edge]. *)
	let farther_dest instance vertex edge =
		let position = (ref 0) in
		let original_dst_pos = (compute_vertex_position position instance vertex) in
		position := 0;
		let edge_dst = !(T.Dep_edge.get_dest edge) in
		let edge_dst_pos = (compute_vertex_position position instance edge_dst) in
		(edge_dst_pos > original_dst_pos)  
		
	(** Scan original [vertex] looking for all outgoing go/blue edges with the 
			 same port and destination farher down w.r.t. the original destination. *) 
	let find_out_edges inst_lines vertex orig_go_edge =
		(* first, keep only edges with same port tag *)
		let all_out_go_edges = (T.Vertex.get_go_edges vertex) in
		let port = (T.Dep_edge.get_port orig_go_edge) in
		let all_out_go_edges_same_port = (List.filter (same_port port) all_out_go_edges) in
		(* then, keep only edges with destination on same instance line as the original one *)
		let go_edge_dst = !(T.Dep_edge.get_dest orig_go_edge) in
		let dst_instance_id = (T.Vertex.get_id go_edge_dst) in
		let all_out_go_edges_same_instance = (List.filter (same_instance dst_instance_id) all_out_go_edges_same_port) in
		(* finally, select edges with destination farther down w.r.t. the original one *)
		let dst_instance = (find_by_id dst_instance_id !inst_lines) in
		let all_out_go_edges_farther_dest = (List.filter (farther_dest dst_instance go_edge_dst) all_out_go_edges_same_instance) in
		let make_edge_src_pair src_vertex edge = (src_vertex, edge) in
		let out_edges_same_vertex = (List.map (make_edge_src_pair vertex) all_out_go_edges_farther_dest) in
 		out_edges_same_vertex 

	(** Scan the instance line of [vertex] looking for vertices farther down the instance line
     	with outgoing go/blue arc(s) tagged by the same port as [go_edge]. *) 
	let find_farther_vertex_edge_pairs vertex go_edge =
		let pairs = (ref []) in
		let current_vertex = (ref vertex) in
		let port = (T.Dep_edge.get_port go_edge) in
		let go_edge_dst = !(T.Dep_edge.get_dest go_edge) in
		let dst_instance_id = (T.Vertex.get_id go_edge_dst) in
		let i = (ref 1) in
		(* scan all successors of the given vertex *)
  	while (T.Vertex.has_successor !current_vertex) do
			begin
				(*(print_endline ("\nLoop iteration nr. " ^ (string_of_int !i)));*)
				(* consider only edges tagged with same port as go_edge *)
				let successor = (T.Vertex.get_succ !current_vertex) in
				let go_edges_same_port = (T.Vertex.filter_go_edges_by_port port successor) in
				(* consider only edges with destination on the same instance line as the one of go_edge *)
				let go_edges_same_instance = (List.filter (same_instance dst_instance_id) go_edges_same_port) in
				if go_edges_same_instance <> [] then 
					begin
						(* add current vertex and all its outgoing edges to the list *)
						let make_edge_src_pair src_vertex edge = (src_vertex, edge) in
						let new_pairs = (List.map (make_edge_src_pair successor) go_edges_same_instance) in
						pairs := new_pairs @ !pairs 
					end;
				i := !i + 1;
				current_vertex := successor
			end
		done;
		!pairs

	(** Delete unnecessary dependency edges: remove each blue/go edge  and its 
			corresponding twin. *) 
	let rec remove_go_and_twin_edges file_buffer go_edges_to_remove =
		match go_edges_to_remove with
			[] -> () 
		| head :: tail -> begin
				let vertex = (fst head) in	
				let go_edge = (snd head) in
				let twin_edge = !(T.Dep_edge.get_twin go_edge) in
				(* remove go/blue edge *)
				(T.Vertex.remove_go_edge file_buffer vertex go_edge);
				(* remove return/red edge *)
				(* first find origin vertex of twin_edge *)
				let go_edge_dst = !(T.Dep_edge.get_dest go_edge) in
				let succ = (T.Vertex.get_succ go_edge_dst) in
				(T.Vertex.remove_return_edge file_buffer succ twin_edge);
				(remove_go_and_twin_edges file_buffer tail) 
			end

	(** Utility function that among the instance lines [inst_lines] finds the one
			that contains the destination of [edge]. *)
	let find_instance_by_edge inst_lines edge =
		let edge_dst = !(T.Dep_edge.get_dest edge) in
		let dst_instance_id = (T.Vertex.get_id edge_dst) in
		let dst_instance = (find_by_id dst_instance_id !inst_lines) in
		dst_instance

	(** Take a vertex-edge pair (v,e) and build a pair ((v,e), pos) where pos = dst(e) 
		is the destination's position. *)
	let make_vertex_pos_pair instance vertex_edge_pair =
	  let edge = (snd vertex_edge_pair) in
		let edge_dst = !(T.Dep_edge.get_dest edge) in
		let init_pos = (ref 0) in
		let dst_position = (compute_vertex_position init_pos instance edge_dst) in
		(vertex_edge_pair, dst_position)
	
	(** Ad-hoc comparison function: vertex with farther destination position get to
		 the head of the list. *)
	let compare_pos pair1 pair2 =
		let pos1 = (snd pair1) in 
		let pos2 = (snd pair2) in 
		let result = (compare pos1 pos2) in
		let revert_result = (ref 0) in
		if result != 0 then 
			revert_result := ~-result;
		!revert_result

	(** Among the vertex-edge pairs [vertex_edge_pairs] find the one with the 
			destination vertex farthest away down the instance line [instance]. *)
	let find_farthest instance vertex_edge_pairs =
		if vertex_edge_pairs = [] then
			raise No_pairs
		else begin
			let vertex_position_pairs = (List.map (make_vertex_pos_pair instance) 
				vertex_edge_pairs) in
			let sorted_vertex_position_pairs = 
				(List.fast_sort compare_pos vertex_position_pairs) in
			let farthest_vertex_edge_pos_pair = (List.hd sorted_vertex_position_pairs) in
			let farthest_vertex_edge_pair = (fst farthest_vertex_edge_pos_pair) in 
			farthest_vertex_edge_pair 
		end

	(** Remove a vertex-edge pair from a list of such pairs. *)
	let rec remove_pair vertex_edge_pair pair_list =
		let eq_vertex_edge pair1 pair2 =
			let vertex1 = (fst pair1) and vertex2 = (fst pair2) in
			let edge1 = (snd pair1) and edge2 = (snd pair2) in
			((T.Vertex.eq_id_tag vertex1 vertex2) && (T.Dep_edge.eq edge1 edge2)) in
		match pair_list with
		  [] -> []
		| head :: tail -> begin
				if (eq_vertex_edge head vertex_edge_pair) then
					(remove_pair vertex_edge_pair tail)
				else
					head :: (remove_pair vertex_edge_pair tail)
			end

	(** Utility function for string representation of a vertex-edge pair. *) 
	let string_of_pair vertex_edge_pair =
		let vertex = (fst vertex_edge_pair) in
		let edge = (snd vertex_edge_pair) in
		let vertex_str = (T.Vertex.to_string_with_id vertex) in 
		let edge_str = (T.Dep_edge.to_string edge) in
		let string_repr = ("(" ^ vertex_str ^ ", " ^ edge_str ^ ")") in
		string_repr

	(** Utility function for string representation of a list of vertex-edge pairs. *) 
	let string_of_pair_list vertex_edge_pairs =
		let string_repr = ref "{ }" in
		if vertex_edge_pairs <> [] then begin
			let string_list = (List.map string_of_pair vertex_edge_pairs) in
    	string_repr := (String.concat "  |  " string_list)
		end;
		!string_repr
	
	(** Deal with pair of edges that overlap on all go edges of a vertex. *)
	let edge_fix_enclosing file_buffer inst_lines vertex go_edge =
		let farther_pairs_same_vertex = (find_out_edges inst_lines vertex go_edge) in
		let farther_pairs = (find_farther_vertex_edge_pairs vertex go_edge) in  
		let all_vertex_edge_pairs = farther_pairs_same_vertex @ farther_pairs in
		IFDEF VERBOSE THEN
			(Printf.bprintf !file_buffer "%s\n" ("\n################# Fix enclosing edges for vertex " 
				^ (T.Vertex.to_string_with_id vertex) ^ " and GO edge " ^ (T.Dep_edge.to_string go_edge)));
			(Printf.bprintf !file_buffer "%s\n" ("Pairs found from the given vertex: " 
				^ (string_of_pair_list farther_pairs_same_vertex)));
			(Printf.bprintf !file_buffer "%s\n" ("Pairs found from successors of the given vertex: " 
				^ (string_of_pair_list farther_pairs))); 
			(Printf.bprintf !file_buffer "%s\n" ("Total set of pairs found: " 
				^ (string_of_pair_list all_vertex_edge_pairs))) 
		END;
		(* find vertex with farthest destination *)
		let dst_instance = (find_instance_by_edge inst_lines go_edge) in
		let farthest_vertex_edge_pair = ref (vertex, go_edge) in
		try begin 
			farthest_vertex_edge_pair := (find_farthest dst_instance all_vertex_edge_pairs);
			(*  keep track of the go edges visited in order to remove them in the end *)
			let go_edges_to_remove = (remove_pair !farthest_vertex_edge_pair all_vertex_edge_pairs) in
			IFDEF VERBOSE THEN
				(Printf.bprintf !file_buffer "%s\n" ("Farthest vertex: " 
					^ (string_of_pair !farthest_vertex_edge_pair)));
				(Printf.bprintf !file_buffer "%s\n" ("Edges to be removed: " 
					^ (string_of_pair_list go_edges_to_remove))) 
			END;
			let farthest_vertex = (fst !farthest_vertex_edge_pair) in 
			let farthest_go_edge = (snd !farthest_vertex_edge_pair) in
			(* check that there are no surprises, i.e. that the destination nodes belong to the same instance line *)		
			let go_edge_dst = !(T.Dep_edge.get_dest go_edge) in
			let farthest_go_edge_dst = !(T.Dep_edge.get_dest farthest_go_edge) in
			if (T.Vertex.not_on_same_instance go_edge_dst farthest_go_edge_dst) then
				raise (Dest_vertices_not_on_same_instance (
					(T.Vertex.to_string_with_id go_edge_dst) ^ " and " ^ 
					(T.Vertex.to_string_with_id farthest_go_edge_dst) ^ "lie on different instance lines.")) 
			else begin
				let orig_twin = !(T.Dep_edge.get_twin go_edge) in	
				let orig_twin_src = (T.Vertex.get_succ go_edge_dst) in
				(* make the farthest return edge twin of the original one and vice versa *) 
				let farthest_twin = !(T.Dep_edge.get_twin farthest_go_edge) in
				(T.Dep_edge.set_twin go_edge farthest_twin);  	
				(T.Dep_edge.set_twin farthest_twin go_edge);
				(* remove the farthest go/blue edge from the farthest vertex *)
				(T.Vertex.remove_go_edge file_buffer farthest_vertex farthest_go_edge);
				(* remove the (return/red) twin edge of the original go_edge *)
				(T.Vertex.remove_return_edge file_buffer orig_twin_src orig_twin);
				(* remove all other edges go/blue and return/red ones *)
				(remove_go_and_twin_edges file_buffer go_edges_to_remove);
				IFDEF VERBOSE THEN
					(Printf.bprintf !file_buffer "%s\n" "Apply EDGE FIXING");
					(Printf.bprintf !file_buffer "%s\n" ("Orig twin edge: " 
						^ (T.Dep_edge.to_string orig_twin) ^ " from " 
						^ (T.Vertex.to_string_with_id orig_twin_src)));
					let farthest_twin_src = (T.Vertex.get_succ farthest_go_edge_dst) in
					(Printf.bprintf !file_buffer "%s\n" ("Farthest twin edge: " 
						^ (T.Dep_edge.to_string farthest_twin) ^ " from " 
						^ (T.Vertex.to_string_with_id farthest_twin_src)));
					(Printf.bprintf !file_buffer "%s\n" ("Instance lines AFTER fixing enclosing edges: " 
						^ (to_string_list_full !inst_lines)))
				END
			end
		end
		with No_pairs -> 
			IFDEF VERBOSE THEN
				(Printf.bprintf !file_buffer "%s\n" 
			 		("DO NOTHING because farthest edge is the SAME as the original edge " ^ (T.Dep_edge.to_string go_edge)))
			END

	(** Deal with pair of edges that overlap on all go edges of [vertex]. *)
	let vertex_fix_enclosing_edges file_buffer inst_lines vertex =
		let go_edges = (T.Vertex.get_go_edges vertex) in
		(List.iter (edge_fix_enclosing file_buffer inst_lines vertex) go_edges) 
		
	(** Deal with pair of edges that overlap on all instance lines. *)
  let fix_enclosing_edges_pairs file_buffer inst_lines = 
  	let instance_fix_enclosing_edges file_buffer ref_inst_lines instance = 
    	(List.iter (vertex_fix_enclosing_edges file_buffer ref_inst_lines) instance.vertices) in        
		(List.iter (instance_fix_enclosing_edges file_buffer (ref inst_lines)) inst_lines)
 
