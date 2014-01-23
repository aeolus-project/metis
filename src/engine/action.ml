
(** Type for the actions associated to every vertex in the instance lines. 
		
		Actions are of five kinds: bind, unbind, new, stateChange and delete.

    @author Tudor A. Lascu 
*)
      
type t =
	New of (string * string)
| State_change of (string * string * string)
| Bind of (string * string * string)
| Unbind of (string * string * string)
| Del of string (* used for tags of the kind (3,D) *)


let to_string action =
	match action with
	  New (inst_name, comp_type_name) -> ("[Create instance " ^ inst_name ^ ":" ^ comp_type_name ^ "]") 
  | State_change (inst_name, src, dst) -> ("[" ^ inst_name ^ " : change state from " ^ src ^ " to " ^ dst ^ "]")
  | Bind (port, provider, requirer) -> ("[" ^ provider ^ " : bind port " ^ port ^ " to instance "^ requirer ^ "]") (* bind performed by provider *)
  | Unbind (port, provider, requirer) -> ("[" ^ requirer ^  " : unbind port " ^ port ^ " from instance "^ provider ^ "]") (* unbind performed by requirer *)
  | (Del inst_name) -> ("[" ^ inst_name ^ " : stop]")
      
let string_of_actions_list actions_list =
	let string_list = (List.map to_string actions_list) in
  (String.concat " " string_list)
