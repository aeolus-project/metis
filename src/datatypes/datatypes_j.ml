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

(** Type definitions for multiple targets specification. *)
type universe = Datatypes_t.universe

type target = Datatypes_t.target = {
  component: component_name;
  state: state_name
}

type multiple_targets = Datatypes_t.multiple_targets

let write_component_type_name = (
  Yojson.Safe.write_string
)
let string_of_component_type_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component_type_name ob x;
  Bi_outbuf.contents ob
let read_component_type_name = (
  Ag_oj_run.read_string
)
let component_type_name_of_string s =
  read_component_type_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_port_name = (
  Yojson.Safe.write_string
)
let string_of_port_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_port_name ob x;
  Bi_outbuf.contents ob
let read_port_name = (
  Ag_oj_run.read_string
)
let port_name_of_string s =
  read_port_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_component_name = (
  Yojson.Safe.write_string
)
let string_of_component_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component_name ob x;
  Bi_outbuf.contents ob
let read_component_name = (
  Ag_oj_run.read_string
)
let component_name_of_string s =
  read_component_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_state_name = (
  Yojson.Safe.write_string
)
let string_of_state_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_state_name ob x;
  Bi_outbuf.contents ob
let read_state_name = (
  Ag_oj_run.read_string
)
let state_name_of_string s =
  read_state_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  Ag_oj_run.write_list (
    write_state_name
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  Ag_oj_run.read_list (
    read_state_name
  )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 = (
  Ag_oj_run.write_list (
    write_port_name
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  Ag_oj_run.read_list (
    read_port_name
  )
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_state_type = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"u_name\":";
    (
      write_state_name
    )
      ob x.u_name;
    if x.u_successors != [] then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"u_successors\":";
      (
        write__1
      )
        ob x.u_successors;
    );
    if x.u_provides != [] then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"u_provides\":";
      (
        write__2
      )
        ob x.u_provides;
    );
    if x.u_requires != [] then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"u_requires\":";
      (
        write__2
      )
        ob x.u_requires;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_state_type ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_state_type ob x;
  Bi_outbuf.contents ob
let read_state_type = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let x =
      {
        u_name = Obj.magic 0.0;
        u_successors = [];
        u_provides = [];
        u_requires = [];
      }
    in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 6 -> (
                if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = '_' then (
                  match String.unsafe_get s (pos+2) with
                    | 'p' -> (
                        if String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'v' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | 'r' -> (
                        if String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'q' && String.unsafe_get s (pos+5) = 'u' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 12 -> (
                if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            let v =
              (
                read_state_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__1
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            )
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__2
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__2
                ) p lb
              in
              Obj.set_field (Obj.repr x) 3 (Obj.repr v);
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 6 -> (
                  if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = '_' then (
                    match String.unsafe_get s (pos+2) with
                      | 'p' -> (
                          if String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'v' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' then (
                            2
                          )
                          else (
                            -1
                          )
                        )
                      | 'r' -> (
                          if String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'q' && String.unsafe_get s (pos+5) = 'u' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' then (
                            3
                          )
                          else (
                            -1
                          )
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 12 -> (
                  if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              let v =
                (
                  read_state_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__1
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              )
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__2
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__2
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 3 (Obj.repr v);
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields [| !bits0 |] [| "u_name" |];
        Ag_oj_run.identity x
      )
)
let state_type_of_string s =
  read_state_type (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Ag_oj_run.write_array (
    write_state_type
  )
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
  Ag_oj_run.read_array (
    read_state_type
  )
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_automaton_type = (
  write__3
)
let string_of_automaton_type ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_automaton_type ob x;
  Bi_outbuf.contents ob
let read_automaton_type = (
  read__3
)
let automaton_type_of_string s =
  read_automaton_type (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_component_type = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"u_cname\":";
    (
      write_component_type_name
    )
      ob x.u_cname;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"u_automaton\":";
    (
      write_automaton_type
    )
      ob x.u_automaton;
    Bi_outbuf.add_char ob '}';
)
let string_of_component_type ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component_type ob x;
  Bi_outbuf.contents ob
let read_component_type = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let x =
      {
        u_cname = Obj.magic 0.0;
        u_automaton = Obj.magic 0.0;
      }
    in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 7 -> (
                if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 11 -> (
                if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'n' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            let v =
              (
                read_component_type_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read_automaton_type
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 7 -> (
                  if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 11 -> (
                  if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'n' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              let v =
                (
                  read_component_type_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read_automaton_type
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields [| !bits0 |] [| "u_cname"; "u_automaton" |];
        Ag_oj_run.identity x
      )
)
let component_type_of_string s =
  read_component_type (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__4 = (
  Ag_oj_run.write_list (
    write_component_type
  )
)
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let read__4 = (
  Ag_oj_run.read_list (
    read_component_type
  )
)
let _4_of_string s =
  read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_universe = (
  write__4
)
let string_of_universe ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_universe ob x;
  Bi_outbuf.contents ob
let read_universe = (
  read__4
)
let universe_of_string s =
  read_universe (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_target = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"component\":";
    (
      write_component_name
    )
      ob x.component;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"state\":";
    (
      write_state_name
    )
      ob x.state;
    Bi_outbuf.add_char ob '}';
)
let string_of_target ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_target ob x;
  Bi_outbuf.contents ob
let read_target = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let x =
      {
        component = Obj.magic 0.0;
        state = Obj.magic 0.0;
      }
    in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 5 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' then (
                  1
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' then (
                  0
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            let v =
              (
                read_component_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read_state_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 5 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              let v =
                (
                  read_component_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read_state_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields [| !bits0 |] [| "component"; "state" |];
        Ag_oj_run.identity x
      )
)
let target_of_string s =
  read_target (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__5 = (
  Ag_oj_run.write_list (
    write_target
  )
)
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let read__5 = (
  Ag_oj_run.read_list (
    read_target
  )
)
let _5_of_string s =
  read__5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_multiple_targets = (
  write__5
)
let string_of_multiple_targets ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_multiple_targets ob x;
  Bi_outbuf.contents ob
let read_multiple_targets = (
  read__5
)
let multiple_targets_of_string s =
  read_multiple_targets (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
