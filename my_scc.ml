
open T
open Graph
open Instance

module Vertex = struct
	type t = T.Vertex.t
	let compare v1 v2 =
		let v1_full_id = (T.Vertex.get_full_id v1) in
		let v2_full_id = (T.Vertex.get_full_id v2) in
		(Pervasives.compare v1_full_id v2_full_id)
  let hash = Hashtbl.hash
  let equal v1 v2 = (T.Vertex.eq_id_tag v1 v2)
end

module G = struct
	type t = T.Vertex.t list
	module V = T.Vertex
	let is_directed = true
	let iter_vertex = List.iter
	let fold_vertex = List.fold_right
	let iter_succ f _ v = 
		let succs = (T.Vertex.compute_all_succs v) in
		List.iter f succs
	let fold_succ f _ v = 
		let succs = (T.Vertex.compute_all_succs v) in
		List.fold_right f succs
end


module Comp = Components.Make(G)

module My_scc : sig
	val compute_scc : Vertex.t list -> int * (G.V.t -> int)  
	val compute_scc_array : Vertex.t list -> G.V.t list array
	val compute_scc_list : Vertex.t list -> G.V.t list list
	val print_scc : Vertex.t list -> unit
	val print_cycles_list : G.V.t list list -> unit
	val string_of_cycles : G.V.t list list -> string
	val find_cycles : Vertex.t list -> Buffer.t ref -> G.V.t list list
	val elim_cycles : Instance.t list -> G.V.t list list -> Buffer.t ref -> Instance.t list 

end = struct

	let string_of_cycles cycles_list =
		let string_repr = (T.Vertex.to_string_list_list cycles_list) in
		string_repr 
	
	let compute_scc vertices =
		let g = vertices in
		let sccs = (Comp.scc g) in
		sccs

	let compute_scc_array vertices =
		let g = vertices in
		let sccs_array = (Comp.scc_array g) in
		sccs_array

	let compute_scc_list vertices =
		let g = vertices in
		let sccs_list = (Comp.scc_list g) in
		sccs_list

	let print_scc vertices = 
		let sccs = (compute_scc_array vertices) in
		let nr_scc = (Array.length sccs) in
		print_endline ("\nCOMPUTED THE STRONGLY CONNECTED COMPONENTS."
			^ " We have " ^ (string_of_int nr_scc) ^ " scc in the given graph");
		for i = 0 to (nr_scc - 1) do
			let current_scc = sccs.(i) in
			print_string ("SCC nr." ^ (string_of_int i) ^ " = ");
			(T.Vertex.print_simple_list current_scc);
			print_string "\n"
		done

	(* analyze cycle to make sure it contains at least an inst edge *) 
	let cycle_contains_inst_edge cycle =
		let rec analyze vertices =
			match vertices with
				[] -> false
			| head :: tail ->
					begin
						if (T.Vertex.has_inst_edge head) then
							let inst_edge = (T.Vertex.get_inst_edge head) in
							let dest_vertex = !(T.Inst_edge.get_dest inst_edge) in
							if (List.memq dest_vertex cycle) then
								true
							else
								(analyze tail)
						else
							(analyze tail)
					end
		in
		let vertices = cycle in
		let result = (analyze vertices) in
		result
		
	(* TODO: verify this test is sound from a scientific point of view *)
	(* test: for us a "true cycle" is a scc s.t. :
	 *   1. length > 1 
	 *   2. it contains at least an instance edge
	 *)	
	let true_cycle scc =
		let result = ((List.length scc) > 1 && (cycle_contains_inst_edge scc)) in
		result
			
	let print_cycles_list cycles = 
		print_endline "\nFound the following CYCLES: ";
		(List.iter T.Vertex.print_simple_list cycles)

	(* this function returns a list of sccs, each of them is a cycle *)		
	let find_cycles vertices file_buffer = 
		let sccs = (compute_scc_list vertices) in
		let true_cycles_list = (List.filter true_cycle sccs) in
		if true_cycles_list  != [] then
		(Printf.bprintf !file_buffer "%s\n" 
			("\n\nFound the following cycles:\n" ^ (string_of_cycles true_cycles_list)));
		true_cycles_list
	
	let fix_copy_edges splitted_inst duplicate_inst instances file_buffer vertex =
		let in_blue_edges_verts_pairs = (Instance.find_in_edges_vertices_pairs vertex instances) in
		let new_dest = (Instance.find_corresponding_vertex vertex splitted_inst !duplicate_inst) in
		(T.Dep_edge.duplicate_blue_red_edges (ref new_dest) in_blue_edges_verts_pairs file_buffer) 
			 
	let fix_copy_edges_inst split_vertex splitted_inst duplicate_inst instances file_buffer =
		(Printf.bprintf !file_buffer "%s\n"	"\nFIX EDGES to/from COPY.");
		(*let duplicate_inst = (Instance.find_by_id dupl_inst_id instances) in*)
		let dupl_inst_length = (Instance.length !duplicate_inst) in
		(* (dupl_inst_length-1) means we ignore the split_vertex (s1,s2) because it corresponds to a final vertex (s1,D) in the duplicate instance *)
		let splitted_vertices = (T.Vertex.chop_list (Instance.get_vertices splitted_inst) (dupl_inst_length-1)) in
		(Printf.bprintf !file_buffer "%s\n" ("The splitted meaningful vertices are the following: " ^ (T.Vertex.to_string_list splitted_vertices)));
		(List.iter (fix_copy_edges splitted_inst duplicate_inst instances file_buffer) splitted_vertices) 
	
	(* function employed to redirect GO/BLUE and RETURN/RED edges towards new/copy
	 * instance *)	
	let adjust_split_edges split_edge split_vertex splitted_inst duplicate_inst instances file_buffer =
		(Printf.bprintf !file_buffer "%s\n"	"\nREDIRECTION OF GO/BLUE and RETURN/RED edges.");
		(* for the red edge we simply change its destination vertex *)
		(*let duplicate_inst = (Instance.find_by_id dupl_inst_id instances) in*)
		let final_vertex = (Instance.get_final_vertex !duplicate_inst) in
		(T.Dep_edge.set_dest split_edge (ref final_vertex));
		(* take care of the nr. of incoming edges *)
		(T.Vertex.decrease_nr_in_edges split_vertex);
		(T.Vertex.increase_nr_in_edges final_vertex);
		(Printf.bprintf !file_buffer "%s\n" ("RETURN/RED edge redirected towards: " 
			^ (T.Dep_edge.to_string split_edge)));
		(* now we take care of the blue one *)
		let orig_instance = (Instance.find_by_vertex split_vertex instances) in
		let orig_blue_edge = (T.Dep_edge.get_twin split_edge) in
		let orig_blue_edge_src = (Instance.find_src_by_go_edge orig_instance !orig_blue_edge) in
		let tag = (T.Vertex.get_tag orig_blue_edge_src) in
		let new_blue_edge_src = (T.Vertex.find_by_tag tag (Instance.get_vertices !duplicate_inst)) in
		(* build a brand new go_edge *)
		let orig_blue_edge_dst = (T.Dep_edge.get_dest !orig_blue_edge) in
		let orig_port = (T.Dep_edge.get_port !orig_blue_edge) in
		let new_blue_edge = (T.Dep_edge.make_go orig_blue_edge_dst orig_port) in 
		(* the newly created edge and the split_edge are twins *)
		(T.Dep_edge.set_mutual_twins split_edge new_blue_edge);
		(* we add the new edge to the vertex in the copy instance 
		 * and we remove the original one *)
		(T.Vertex.add_go_edge new_blue_edge_src new_blue_edge);
		(Printf.bprintf !file_buffer "%s\n"	("New GO/BLUE edge: " 
			^ (T.Vertex.to_string_with_id new_blue_edge_src) ^ " " 
			^ (T.Dep_edge.to_string new_blue_edge)));
		(T.Vertex.remove_go_edge orig_blue_edge_src !orig_blue_edge);
		(Printf.bprintf !file_buffer "%s\n" ("Old GO/BLUE edge that has been removed: " 
			^ (T.Vertex.to_string_with_id orig_blue_edge_src) ^ " " 
			^ (T.Dep_edge.to_string !orig_blue_edge)));
		(* finally every GO/BLUE edge incoming into new_blue_edge_src must also be 
			duplicated together with its corresponding RETURN/RED edge *) 
		(fix_copy_edges_inst split_vertex splitted_inst duplicate_inst instances file_buffer)

	(* instances is a reference to a list of instance lines *)
	let elim_single_cycle instances cycle file_buffer =
		(* find splitting edge and corresponding vertex *)
		let splitting_edge = (T.Vertex.find_split_edge cycle) in
		let splitting_vertex = !(T.Dep_edge.get_dest splitting_edge) in
		(Printf.bprintf !file_buffer "%s\n" ("\nVertex chosen for splitting: " 
			^ (T.Vertex.to_string_with_id splitting_vertex)));
		(* find instance line to which splitting vertex belongs *)
		let instance_line_to_split = (Instance.find_by_vertex splitting_vertex instances) in
		(Printf.bprintf !file_buffer "%s\n" ("\nThe instance line to be split must be:\n" 
			^ (Instance.to_string instance_line_to_split)));
		(* duplicate_instance is the one on which we'll redirect the GO and RETURN edges *)
		let duplicate_instance = (Instance.copy_line_until_vertex instance_line_to_split splitting_vertex) in
		(Printf.bprintf !file_buffer "%s\n"	("\nCopy instance line:\n" 
			^ (Instance.to_string duplicate_instance)));
		(* we add destination instance to the set of instance lines *)	
		let new_instances = duplicate_instance :: instances in
		(Printf.bprintf !file_buffer "%s\n"	("\nInstance lines updated to: \n" 
			^ (Instance.to_string_list new_instances)));
		(* move blue and red edges towards copy *)
		(*let duplicate_instance_id = (Instance.get_id duplicate_instance) in*)
		(adjust_split_edges splitting_edge splitting_vertex instance_line_to_split 
			(ref duplicate_instance) new_instances file_buffer);
		new_instances
		
	let rec elim_cycles instances cycles file_buffer =
		match cycles with
			[] -> instances
		| head :: tail ->
				begin
					let current_cycle = head in
					let new_instances = (elim_single_cycle instances current_cycle file_buffer) in
					let new_vertices = (Instance.list_to_vertices new_instances) in
					let new_cycles = (find_cycles new_vertices file_buffer) in
					(elim_cycles new_instances new_cycles file_buffer)
				end
	
(* version which tries to reuse already existing instance lines 
	by means of function:	Instance.find_ready_iline
	(* instances is a reference to a list of instance lines *)
	let elim_single_cycle_DEBUG instances cycle file_buffer =
		(* find splitting edge and corresponding vertex *)
		let splitting_edge = (T.Vertex.find_split_edge cycle) in
		let splitting_vertex = !(T.Dep_edge.get_dest splitting_edge) in
		(Printf.bprintf !file_buffer "%s\n" ("\nVertex chosen for splitting: " 
			^ (T.Vertex.to_string_with_id splitting_vertex)));
		(* find instance line to which splitting vertex belongs *)
		let instance_line_to_split = (Instance.find_by_vertex splitting_vertex instances) in
		(Printf.bprintf !file_buffer "%s\n" ("\nThe instance line to be split must be:\n" 
			^ (Instance.to_string instance_line_to_split)));
		(* destination_instance is the one on which we'll redirect the GO and RETURN edges *)
		let destination_instance = ref (Instance.copy_line_until_vertex instance_line_to_split splitting_vertex) in
		(Printf.bprintf !file_buffer "%s\n"	("\nCopy instance line:\n" 
			^ (Instance.to_string !destination_instance)));
		(* if (there already exists an instance line identical to the splitting one) 
		 * then no need to duplicate *)
		try 
			let right_iline = (Instance.find_ready_iline splitting_vertex instances) in
			if (Instance.eq !destination_instance right_iline) then
				begin
					(Printf.bprintf !file_buffer "%s\n"	("\nNo need to split any instance line. We can use the following one:\n" 
						^ (Instance.to_string right_iline)));
					(* move blue and red edges towards already existing instance line *)
					(adjust_split_edges_DEBUG splitting_edge splitting_vertex right_iline instances);
				end
			else
					(Printf.bprintf !file_buffer "%s\n"	("\nI DON'T KNOW WHAT HAPPENED. I found:\n" 
						^ (Instance.to_string right_iline)));
			instances
		with Not_found -> 
			(* else we need to add destination instance to the set of instance lines *)	
			begin	
				let new_instances = !destination_instance :: instances in
				(Printf.bprintf !file_buffer "%s\n"	("\nInstance lines updated to: \n" 
					^ (Instance.to_string_list new_instances)));
				(* move blue and red edges towards copy *)
				(adjust_split_edges_DEBUG splitting_edge splitting_vertex !destination_instance new_instances);
				new_instances
			end
*)

end	

