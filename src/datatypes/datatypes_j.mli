(* Auto-generated from "datatypes.atd" *)


(** Type definitions for naming. *)

type component_type_name = Datatypes_t.component_type_name

type port_name = Datatypes_t.port_name

type component_name = Datatypes_t.component_name

(** Type definitions for Component Type. *)
type state_name = Datatypes_t.state_name

type state_type = Datatypes_t.state_type = {
  u_name: state_name;
  u_successors: state_name list;
  u_provides: port_name list;
  u_requires: port_name list
}

type automaton_type = Datatypes_t.automaton_type

(** Type definitions for Universe. *)
type component_type = Datatypes_t.component_type = {
  u_cname: component_type_name;
  u_automaton: automaton_type
}

type universe = Datatypes_t.universe

val write_component_type_name :
  Bi_outbuf.t -> component_type_name -> unit
  (** Output a JSON value of type {!component_type_name}. *)

val string_of_component_type_name :
  ?len:int -> component_type_name -> string
  (** Serialize a value of type {!component_type_name}
      into a JSON string.
      @param len specifies the initial length 
                 of the buffer used internally.
                 Default: 1024. *)

val read_component_type_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component_type_name
  (** Input JSON data of type {!component_type_name}. *)

val component_type_name_of_string :
  string -> component_type_name
  (** Deserialize JSON data of type {!component_type_name}. *)

val write_port_name :
  Bi_outbuf.t -> port_name -> unit
  (** Output a JSON value of type {!port_name}. *)

val string_of_port_name :
  ?len:int -> port_name -> string
  (** Serialize a value of type {!port_name}
      into a JSON string.
      @param len specifies the initial length 
                 of the buffer used internally.
                 Default: 1024. *)

val read_port_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> port_name
  (** Input JSON data of type {!port_name}. *)

val port_name_of_string :
  string -> port_name
  (** Deserialize JSON data of type {!port_name}. *)

val write_component_name :
  Bi_outbuf.t -> component_name -> unit
  (** Output a JSON value of type {!component_name}. *)

val string_of_component_name :
  ?len:int -> component_name -> string
  (** Serialize a value of type {!component_name}
      into a JSON string.
      @param len specifies the initial length 
                 of the buffer used internally.
                 Default: 1024. *)

val read_component_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component_name
  (** Input JSON data of type {!component_name}. *)

val component_name_of_string :
  string -> component_name
  (** Deserialize JSON data of type {!component_name}. *)

val write_state_name :
  Bi_outbuf.t -> state_name -> unit
  (** Output a JSON value of type {!state_name}. *)

val string_of_state_name :
  ?len:int -> state_name -> string
  (** Serialize a value of type {!state_name}
      into a JSON string.
      @param len specifies the initial length 
                 of the buffer used internally.
                 Default: 1024. *)

val read_state_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> state_name
  (** Input JSON data of type {!state_name}. *)

val state_name_of_string :
  string -> state_name
  (** Deserialize JSON data of type {!state_name}. *)

val write_state_type :
  Bi_outbuf.t -> state_type -> unit
  (** Output a JSON value of type {!state_type}. *)

val string_of_state_type :
  ?len:int -> state_type -> string
  (** Serialize a value of type {!state_type}
      into a JSON string.
      @param len specifies the initial length 
                 of the buffer used internally.
                 Default: 1024. *)

val read_state_type :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> state_type
  (** Input JSON data of type {!state_type}. *)

val state_type_of_string :
  string -> state_type
  (** Deserialize JSON data of type {!state_type}. *)

val write_automaton_type :
  Bi_outbuf.t -> automaton_type -> unit
  (** Output a JSON value of type {!automaton_type}. *)

val string_of_automaton_type :
  ?len:int -> automaton_type -> string
  (** Serialize a value of type {!automaton_type}
      into a JSON string.
      @param len specifies the initial length 
                 of the buffer used internally.
                 Default: 1024. *)

val read_automaton_type :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> automaton_type
  (** Input JSON data of type {!automaton_type}. *)

val automaton_type_of_string :
  string -> automaton_type
  (** Deserialize JSON data of type {!automaton_type}. *)

val write_component_type :
  Bi_outbuf.t -> component_type -> unit
  (** Output a JSON value of type {!component_type}. *)

val string_of_component_type :
  ?len:int -> component_type -> string
  (** Serialize a value of type {!component_type}
      into a JSON string.
      @param len specifies the initial length 
                 of the buffer used internally.
                 Default: 1024. *)

val read_component_type :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component_type
  (** Input JSON data of type {!component_type}. *)

val component_type_of_string :
  string -> component_type
  (** Deserialize JSON data of type {!component_type}. *)

val write_universe :
  Bi_outbuf.t -> universe -> unit
  (** Output a JSON value of type {!universe}. *)

val string_of_universe :
  ?len:int -> universe -> string
  (** Serialize a value of type {!universe}
      into a JSON string.
      @param len specifies the initial length 
                 of the buffer used internally.
                 Default: 1024. *)

val read_universe :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> universe
  (** Input JSON data of type {!universe}. *)

val universe_of_string :
  string -> universe
  (** Deserialize JSON data of type {!universe}. *)

