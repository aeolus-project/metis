(** The purpose of this module is to replicate the component types changing the port names according to some bindings
*)

open Facade
open My_datatypes
open Json_zephyrous_output_t

module StringSet = Set.Make(String) ;;
let compose f g = function x -> f (g x)

let delete_duplicates lst =
  let seen = Hashtbl.create (List.length lst) in
  List.filter (fun x -> let tmp = not (Hashtbl.mem seen x) in
                        Hashtbl.replace seen x ();
                        tmp) lst

let copy_state state =
	let new_state = {
  	id = state.id;
    successors = state.successors;
    provides = List.fold_right (function x -> (function y -> x::y)) state.provides [];
    requires = List.fold_right (function x -> (function y -> x::y)) state.requires []
	} in
	new_state

(* Change the component type name using the zephyrus component name  *)
let change_component_type_name component_type conf_component =
	let new_component_type = {
		cname = conf_component.component_name;
  	automaton = Array.map copy_state component_type.automaton
	} in
	new_component_type

(* Produce a list of new component types (one for every zephyrus component, changing their names  *)
let get_component_types universe real_components =
	let get_component_types real_comp = List.find (function x -> x.cname = real_comp.component_type) universe in
		let newtypes = ( List.map2
  		change_component_type_name
  		(List.map get_component_types real_components)
  		real_components ) in			
			(* print_endline ("\n New types:\n" ^ (Facade.string_of_universe newtypes)); *)
			newtypes

(* Change the port name replacing them with the values contained in a hash map if they are present *)
let change_strings lst hash =
	let s = ref StringSet.empty in
	List.iter (function x ->
		if Hashtbl.mem hash x then
			StringSet.iter (function y ->
				s := (StringSet.add (String.concat "_" [x;y]) !s)) (Hashtbl.find hash x)
		else
			s := (StringSet.add x !s)
		) lst;
	StringSet.elements !s

(* Change the port names of the components according to the bindings. *)
(* Provide port p of x is replaced with p_x *)
(* Require port p that requires x_1, x_2 is replaced by two ports x_1 and x_2 *)
let change_port component bindings =
	let provide_bindings = Hashtbl.create (List.length bindings) in
	let require_bindings = Hashtbl.create (List.length bindings) in
	List.iter (function (x : binding) -> 
		if x.binding_provider = component.cname then
			if not (Hashtbl.mem provide_bindings x.binding_port) then
				Hashtbl.add provide_bindings x.binding_port (StringSet.singleton x.binding_provider)
		) bindings;
	List.iter (function x ->
		if x.binding_requirer = component.cname then
			if Hashtbl.mem require_bindings x.binding_port then
				Hashtbl.replace require_bindings x.binding_port
					(StringSet.add x.binding_provider (Hashtbl.find require_bindings x.binding_port)) 
			else
				Hashtbl.add require_bindings x.binding_port (StringSet.singleton x.binding_provider)
		) bindings;
	Array.iter (function x -> 
		x.provides <- (change_strings x.provides provide_bindings);
		x.requires <- (change_strings x.requires require_bindings)
		) component.automaton
		
let combine_universe_configurator universe conf =
	let new_component_types = get_component_types universe conf.configuration_components in
	List.iter (function x -> change_port x conf.configuration_bindings) new_component_types;
	List.append universe new_component_types

(* Extract the targets from the configuration		 *)
let get_targets conf =
	List.map (fun x -> (x.component_name,x.component_state)) conf.configuration_components

		

	
		
		
	
	
