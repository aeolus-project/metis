
open My_datatypes
open Datatypes_t
open Gg
open T
  
(********************************************************************)
(*			            Module for instance lines                 			*)
(********************************************************************)

type t
val length : t -> int
val eq : t -> t -> bool
val get_comp_type : t -> component_t ref
val get_id : t -> string
val get_vertices : t -> T.Vertex.t list
val set_vertices : t -> T.Vertex.t list -> unit
val rev_vertices : t -> unit
val to_string : t -> string
val to_string_list : t list -> string
val to_string_full : t -> string
val to_string_list_full : t list -> string
val print_list : Buffer.t ref -> t list -> unit
val make : (component_t ref) -> string -> t
val add_vertex : t -> T.Vertex.t -> unit  
(* generates instance lines from a list of G-nodes *)
val build_instance_lines : (Gg.Node.t list) list -> t list
val filter_by_comp_type : (component_t ref) -> t list -> t list
val list_add_dep_edges : t list -> unit
val fix_enclosing_edges_pairs : t list -> unit
val list_to_vertices : t list -> (T.Vertex.t list)
val filter_maximal_paths : (Gg.Node.t list) list -> (Gg.Node.t list) list 
(* functions involved in the splitting phase *)
val get_final_vertex : t -> T.Vertex.t 
val copy_line_until_vertex : t -> T.Vertex.t -> t
val find_by_vertex : T.Vertex.t -> t list -> t 
val find_by_id : string -> t list -> t 
(*
val find_ready_iline : T.Vertex.t -> t list -> t
*)
val find_src_by_go_edge : t -> T.Dep_edge.t -> T.Vertex.t 
val find_in_edges_vertices_pairs : T.Vertex.t -> t list -> (T.Dep_edge.t * T.Vertex.t) list 
val find_corresponding_vertex : T.Vertex.t -> t -> t -> T.Vertex.t  
val find_farthest_vertex_edge : T.Vertex.t -> T.Dep_edge.t -> ((T.Vertex.t * T.Dep_edge.t ) list) ref -> T.Vertex.t * T.Dep_edge.t 

