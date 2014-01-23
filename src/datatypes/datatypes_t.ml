(* Auto-generated from "datatypes.atd" *)


(** Type definitions for naming. *)

type component_type_name = string

type port_name = string

type component_name = string

(** Type definitions for Component Type. *)
type state_name = string

type state_type = {
  u_name: state_name;
  u_successors: state_name list;
  u_provides: port_name list;
  u_requires: port_name list
}

type automaton_type = state_type array

(** Type definitions for Universe. *)
type component_type = {
  u_cname: component_type_name;
  u_automaton: automaton_type
}

type universe = component_type list
