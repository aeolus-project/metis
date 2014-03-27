
(** These datatypes are tuned ones that transform the original ones found in
		datatypes_t.ml (generated from datatypes.atd).
		A state type (here state_t) contains now also a key (an int) used to 
  	identify it in the automaton_t (implemented with an array).
  	This additional field is set by traversing the universe specified by the
  	user.  
  	This change affects waterfall the other data structures: automaton, 
  	component and universe.
 *)

open Datatypes_t

(** Definition of state ID type. 
		The type for state ID is basically a [name] plus a [key], corresponding to 
		its position in the array representation of the automaton. 
	*)
type state_id_t = {
	key: int;
  value: state_name
}

(** Definition of state type. *)
type state_t = {
  id: state_id_t;
  mutable successors: state_id_t list;
  provides: port_name list;
  requires: port_name list
}

(** Definition of automaton type. *)
type automaton_t = state_t array

(** Definition of component type. *)
type component_t = {
  cname: component_type_name;
  automaton: automaton_t
}

(** Definition of universe type. *)
type universe_t = component_t list
