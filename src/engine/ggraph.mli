
open My_datatypes
open Datatypes_t
open Generation
open Gg


(****************************************************************************************)
(*					G-graph						*)
(****************************************************************************************)
(*
module type GGRAPH = 
  sig
*)          
    	type t
      val get_target : t -> Gg.Node.t
		  val get_generations : t -> (Generation.t list)
      (* used to update target field if/when we find_in_list it in the initial top-down phase *)
      val set_target : t -> Gg.Node.t -> unit
      val set_generations : t -> (Generation.t list) -> unit
      val create : My_datatypes.universe_t -> (My_datatypes.component_t ref) -> string -> t
      val generations_num : t -> int
      val nth_generation : t -> int -> Generation.t
      val add_generation : t -> Generation.t -> unit 
      val print_generations : t -> Buffer.t ref -> unit
      val print_generations_full : t -> unit
		  (* build first generation of the G-graph: all components in their initial state *)
      val build_initial_gen : t -> Generation.t
		  (* generate the G-graph *)
      val populate : t -> unit
      val populate_DEBUG : t -> Buffer.t ref -> unit
      val populate_CHECK : t -> Buffer.t ref -> unit
      (* working with an array of generations, instead of the full graph *)
      val visit : Buffer.t ref -> t -> (Gg.Node.t list) array
      val linearize : (Gg.Node.t list) array -> Gg.Node.t -> (Gg.Node.t list) list
      val print_generations_a : (Gg.Node.t list) array -> unit
      val print_generations_full_a : (Gg.Node.t list) array -> unit
