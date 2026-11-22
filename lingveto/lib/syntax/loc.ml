(* lib/syntax/loc.ml *)

type t = {
  start_pos : Lexing.position;
  end_pos : Lexing.position;
}

let pp fmt { start_pos = s; end_pos = e } =
  let start_col = s.pos_cnum - s.pos_bol + 1 in
  let end_col = e.pos_cnum - e.pos_bol in
  Format.fprintf fmt "%s:%d:%d-%d:%d"
    s.pos_fname s.pos_lnum start_col e.pos_lnum end_col

let equal 
    { start_pos = s1; end_pos = e1 } 
    { start_pos = s2; end_pos = e2 } =
  s1 = s2 && e1 = e2

let make p1 p2 = { start_pos = p1; end_pos = p2 }
