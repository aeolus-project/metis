
open Datatypes_t
open Gg

(********************************************************)
(*			Generation			*)
(********************************************************)

(* an indexed generation is simply a list of nodes plus a level index *)
      type t = {
        index : int; 
        mutable nodes : Gg.Node.t list;
      }        

let get_index gen = 
  gen.index

let get_nodes gen = 
  gen.nodes

let set_nodes gen newNodes = 
  gen.nodes <- newNodes

let add_node gen node = 
  gen.nodes <- node :: gen.nodes

let create_empty = 
  { index = 0; nodes = [] }

let create_with_index i = 
  { index = i; nodes = [] }

let create_with_index_and_nodes i inodes = 
  { index = i; nodes = inodes }

(* This method computes the new nodes originated from the ones in the current 
	generation. It returns the list of new nodes that are not in the current 
	generation. *)
(* New version that computes and updates cardinality and distance info *)
let compute_new_nodes currentGen =
  let currentNodes = currentGen.nodes in 
	let provides = (Gg.Node.provides_of_node_list currentNodes) in 
	(* for every node in current generation compute its successors whose requires are fulfilled by current provides *)
  let successorsList = (Gg.Node.build_succs_list provides currentNodes) in
	(* same node reached by different paths is compactified into a single representative with all possible predecessors *)
	let successors = (Gg.Node.unify_successors successorsList) in
	(* compute the "really" new nodes *)
	let newNodes = (List.filter (fun node -> Gg.Node.not_in_list node currentNodes) successors) in
  newNodes

let to_string gen =
	let init_string = ("Generation nr." ^ (string_of_int gen.index) ^ ": ") in
  let nodes_string_list = (List.map Gg.Node.to_string gen.nodes) in
  let string_repr = init_string ^ (String.concat "; " nodes_string_list) in
  string_repr  

let to_string_full gen =
	let init_string = ("Generation nr." ^ (string_of_int gen.index) ^ ": ") in
  let nodes_string_list = (List.map Gg.Node.to_string_full gen.nodes) in
  let string_repr = init_string ^ (String.concat "; " nodes_string_list) in
  string_repr  
 
let print file_buffer gen = 
	let string_repr = (to_string gen) in
	(Buffer.add_string !file_buffer (string_repr ^ "\n"))

let print_on_screen gen = 
	let string_repr = (to_string gen) in
	(print_endline string_repr)
	 
let print_full gen = 
  print_endline (to_string_full gen)
