(* The purpos of this small module is to translate a universe as specified by 
 * the user into the representation used by the planner (where a state is 
 * identified both by a name and by a key (int) value. This value is 
 * computed and associated by means of the functions here.
 *)

open Facade
open Datatypes_t
open My_datatypes
              
exception Position_not_found of string

(** Build a state ID given a key and a name. *)
let make_state_id new_key new_name =
	let new_state_id = {
				key = new_key;
				value = new_name 
			} in
	new_state_id

(** Translate a state as specified by the user into the one that fits the 
		programs' needs. *)

let find_position_by_name name automaton =
	let length = (Array.length automaton) in
	let index = (ref length) in
	for i = 0 to (length-1) do
		let state = automaton.(i) in
		if (state.id).value = name then 
			index := (state.id).key
	done;
	if (!index = length) then
		raise (Position_not_found ("I was not able to find position of state " 
																^ name ^ " in the automaton."))
	else
		!index 

let translate_succs automaton successors =
	let translate_successor automaton succ_name =
		let succ_pos = (find_position_by_name succ_name automaton) in
		let succ_id = (make_state_id succ_pos succ_name) in
		succ_id
	in
	let succs = (List.map (translate_successor automaton) successors) in
	succs

let translate_automaton user_automaton =
	(* first transform name in ID = (key,name) *)
	let automaton_with_ids_list = (ref []) in
	for i = 0 to ((Array.length user_automaton)-1) do
		let user_state = user_automaton.(i) in
		let state = {
					id = { key = i; value = user_state.u_name };
					successors = [];
					provides = user_state.u_provides;
					requires = user_state.u_requires
		} in
		automaton_with_ids_list := state :: !automaton_with_ids_list
	done; 
	let res_automaton = (Array.of_list (List.rev !automaton_with_ids_list)) in
	(*print_endline ("\n RES_AUTOMATON: " ^ (Facade.string_of_automaton res_automaton));*)
	(* then apply same tranformation to successors *)
	for i = 0 to ((Array.length res_automaton)-1) do
		let state = res_automaton.(i) in
		let user_state = user_automaton.(i) in
		state.successors <- (translate_succs res_automaton user_state.u_successors)
	done;
	res_automaton
	
	
let translate_component user_component =
	let res_automaton = (translate_automaton user_component.u_automaton) in 
	let res_component = {
				cname = user_component.u_cname;	
				automaton = res_automaton
	} in
	res_component	

let translate user_universe =
	let universe = (List.map translate_component user_universe) in
	(*
	print_endline ("\n UNIVERSE GIVEN BY USER:\n" ^ (Facade.string_of_u_universe user_universe));
	print_endline ("\n TRANSLATED UNIVERSE:\n" ^ (Facade.string_of_universe universe));
	*)
	universe

