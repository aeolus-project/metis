
(** Type for plans.

		A plan is simply a (dynamically resizable) array of actions. 

    @author Tudor A. Lascu 
*)

open ExtLib


(** Plan type. *)
type t = {
	mutable actions : Action.t DynArray.t; (** Field [actions] is a dynamically resizable array of actions. *)
}


let length plan =
	(DynArray.length plan.actions)

let to_string ~mandriva_mode plan =
	let actions = plan.actions in
	let plan_string = ref "" in
	(* choose the right function whether in mandriva mode or not *)
	let to_string_fun = match mandriva_mode with
		true -> Action.to_string_mandriva_mode
	| false -> Action.to_string
	in
	for i = 0 to ((DynArray.length actions) - 1) do
		let current_action = (DynArray.get actions i) in
		let action_string = "\n Plan[" ^ (string_of_int (i+1)) ^ "] = " 
			^ (to_string_fun current_action) in
		plan_string := !plan_string ^ action_string   
	done;
	!plan_string

let print ~mandriva_mode plan =
	let string_repr = ("PLAN: " ^ (to_string mandriva_mode plan)) in
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

let add ~mandriva_mode file_buffer plan action =
	(DynArray.add plan.actions action);
	IFDEF VERBOSE THEN
		(* choose the right function if working in mandriva mode or not *)
		let to_string_fun = match mandriva_mode with
			true -> Action.to_string_mandriva_mode
		| false -> Action.to_string
		in
		let action_string = "\n Plan[" ^ (string_of_int (length plan)) ^ "] = " 
			^ (to_string_fun action) in
    (Printf.bprintf !file_buffer "%s\n" action_string)
  END
		
