
(** Module used to deal with state, component and universe   
	*)

open My_datatypes
open Datatypes_t

(* Functions dealing with state *)
val get_state_id : state_t -> state_id_t
val get_state_key : state_t -> int
val get_state_successors : state_t -> state_id_t list
val get_state_provides : state_t -> port_name list
val get_state_requires : state_t -> port_name list
val to_string_state : state_t -> string
val string_of_state_id : state_id_t -> string
val to_int_state : state_t -> int
val is_initial_state : state_t -> bool
val string_of_state : state_t -> string
val string_of_state_full : state_t -> string
val string_of_automaton : automaton_t -> string
val string_of_component : component_t -> string
val string_of_universe : universe_t -> string
val print_state : state_t -> unit
val print_automaton : automaton_t -> unit
val print_component : component_t -> unit
val print_universe : universe_t -> unit

(* Functions dealing with state, component and universe as specified by the user *)
val string_of_u_state : state_type -> string
val string_of_u_automaton : automaton_type -> string
val string_of_u_component : component_type -> string
val string_of_u_universe : universe -> string
val print_u_state : state_type -> unit
val print_u_automaton : automaton_type -> unit
val print_u_component : component_type -> unit
val print_u_universe : universe -> unit

(* Functions dealing with universe *)
val find_component_by_name : universe_t -> string -> component_t
