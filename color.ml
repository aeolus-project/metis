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


