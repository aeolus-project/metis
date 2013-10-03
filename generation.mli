
open Datatypes_t
open Gg

(********************************************************)
(*			Generation			*)
(********************************************************)
      type t
      val get_index : t -> int
      val get_nodes : t -> Gg.Node.t list
      val add_node : t -> Gg.Node.t -> unit 
      val set_nodes : t -> Gg.Node.t list -> unit
      val create_empty : t
      val create_with_index : int -> t
      val create_with_index_and_nodes : int -> Gg.Node.t list -> t
      val compute_new_nodes : t -> Gg.Node.t list
      val old_compute_new_nodes : t -> Gg.Node.t list
      val compute_new_nodes_DEBUG : t -> Gg.Node.t list
      val to_string : t -> string
      val to_string_full : t -> string
      val print : Buffer.t ref -> t -> unit
      val print_on_screen : t -> unit
      val print_full : t -> unit
