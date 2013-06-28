
open My_datatypes
open Datatypes_t

(* Functions dealing with state *)

let get_state_id state =
    state.id 

let get_state_key state =
    (state.id).key 

let get_state_successors state =
    state.successors 

let get_state_provides state =
    state.provides

let get_state_requires state =
    state.requires 

let to_string_state state =
	(state.id).value
	(*
	let key = (state.id).key in
	let value = (state.id).value in
	let string_repr = "(" ^ (string_of_int key) ^ ", " ^ value ^ ")" in
	string_repr
	*)

let string_of_state = to_string_state

(* use the (key,value) pair *)
let old_string_of_state_full state =
	let key = (state.id).key in
	let value = (state.id).value in
	let string_repr = "(" ^ (string_of_int key) ^ ", " ^ value ^ ")" in
	string_repr

let string_of_state_id sid =
	let key = (sid).key in
	let value = (sid).value in
	let string_repr = "(" ^ (string_of_int key) ^ "," ^ value ^ ")" in
	string_repr
	
let string_of_state_full state =
	let key = (state.id).key in
	let value = (state.id).value in
	let id_string = "(" ^ (string_of_int key) ^ "," ^ value ^ ")" in
	let succs_string_list = (List.map string_of_state_id state.successors) in
	let succs_string = (" succs: " ^ "{ " ^ (String.concat " " succs_string_list) 
			^ " }") in
	let provides_string = (" provides: " ^ "{ " 
			^ (String.concat " " state.provides) ^ " }") in
	let requires_string = (" requires: " ^ "{ " 
			^ (String.concat " " state.requires) ^ " }") in
	let string_repr = (id_string ^ succs_string ^ provides_string 
			^ requires_string) in
	string_repr 
	 
let string_of_state_id state_id =
	let key = state_id.key in
	let value = state_id.value in
	let string_repr = "(" ^ (string_of_int key) ^ ", " ^ value ^ ")" in
	string_repr

let to_int_state state =
  (state.id).key

let is_initial_state state =
	match (state.id).key with
		0 -> true
  | _ -> false

let string_of_automaton automaton =
	let string_array = (Array.map string_of_state_full automaton) in
	let string_list = (Array.to_list string_array) in
	let string_repr = ("[ " ^ (String.concat "\n " string_list) ^ " ]") in
	string_repr	

let string_of_component component =
	let name_string = ("component name: " ^ component.cname) in
	let automaton_string = ("automaton :"	
		^ (string_of_automaton component.automaton)) in
	let string_repr =  name_string ^ "\n\n " ^ automaton_string in
	string_repr

let string_of_universe universe =
	let string_list = (List.map string_of_component universe) in
	let string_repr = (String.concat "\n\n\n" string_list) in
	string_repr
	 
let print_state state =
	print_endline (string_of_state state)

let print_automaton automaton =
	print_endline (string_of_automaton automaton)

let print_component component =
	print_endline (string_of_component component)

let print_universe universe =
	print_endline (string_of_universe universe)

(* Functions dealing with universe *)

let find_component_by_name universe name =
  let match_comp_name name component =
    (component.cname = name) in  
  try      
    let component = (List.find (match_comp_name name) universe) in
    component
  with 
    Not_found -> raise Not_found (* TODO: change into global abort *)  


(*********************************************************************************)
(* Functions dealing with state, component and universe as specified by the user *)
(*********************************************************************************)

let string_of_u_state u_state =
	u_state.u_name

let string_of_u_automaton u_automaton =
	let string_array = (Array.map string_of_u_state u_automaton) in
	let string_list = (Array.to_list string_array) in
	let string_repr = ("[ " ^ (String.concat ", " string_list) ^ " ]") in
	string_repr	

let string_of_u_component u_component =
	let name_string = ("component name: " ^ u_component.u_cname) in
	let automaton_string = ("automaton :"	
		^ (string_of_u_automaton u_component.u_automaton)) in
	let string_repr =  name_string ^ "\n " ^ automaton_string in
	string_repr

let string_of_u_universe u_universe =
	let string_list = (List.map string_of_u_component u_universe) in
	let string_repr = (String.concat "\n\n" string_list) in
	string_repr

let print_u_state u_state =
	print_endline (string_of_u_state u_state)

let print_u_automaton u_automaton =
	print_endline (string_of_u_automaton u_automaton)

let print_u_component u_component =
	print_endline (string_of_u_component u_component)

let print_u_universe u_universe =
	print_endline (string_of_u_universe u_universe)

