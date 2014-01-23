
(** My own kind of loop constructs: repeat-until and do-while.

    @author Tudor A. Lascu 
*)


(* my own repeat-until construct *)
let repeat_until f p ~init =
  let rec loop v =
    let v = f v in
    if not(p v) then loop v
  in
  loop init

(* my own do-while construct *)
let do_while f p ~init =
  let rec loop v =
    let v = f v in
    if p v then loop v
  in
  loop init
