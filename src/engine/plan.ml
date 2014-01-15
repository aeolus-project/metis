
(** Type for plans.

		A plan is an (dynamically resizable) array of actions plus an index to the 
		last added action. 

    @author Tudor A. Lascu 
*)

(*
open Batteries
open Batteries_uni
*)
open ExtLib


(** Plan type. *)
type t = {
	mutable actions : Action.t DynArray.t; (** Field [actions] is a dynamically resizable array of actions. *)
}


let length plan =
	(DynArray.length plan.actions)

let old_to_string plan =
	let actions_list = (DynArray.to_list plan.actions) in
		let actions_list_str = (List.map Action.to_string actions_list) in
			let actions_string_repr = (String.concat " " actions_list_str) in 
			actions_string_repr

let to_string plan =
	let actions = plan.actions in
	let plan_string = ref "" in
	for i = 0 to ((DynArray.length actions) - 1) do
		let current_action = (DynArray.get actions i) in
		let action_string = "\n Plan[" ^ (string_of_int (i+1)) ^ "] = " 
			^ (Action.to_string current_action) in
		plan_string := !plan_string ^ action_string   
	done;
	!plan_string

let print plan =
	let string_repr = ("PLAN: " ^ (to_string plan)) in
	(print_endline string_repr)   

let get_actions plan = plan.actions

let make init_num_actions = 
	let new_plan = {
		actions = DynArray.make init_num_actions;
	} in
	new_plan

let get_action plan pos =
	(DynArray.get plan.actions pos)

let set_action plan pos action = 
	(DynArray.set plan.actions pos action)

let insert_action plan pos action = 
	(DynArray.insert plan.actions pos action)

let add plan action =
	(DynArray.add plan.actions action);
	(print_endline ("Plan[" ^ (string_of_int (length plan) ^ "] = " ^ (Action.to_string action))))
		
