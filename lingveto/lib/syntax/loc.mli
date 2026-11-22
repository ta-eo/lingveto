(* lib/syntax/loc.mli *)

type t = {
  start_pos : Lexing.position;
  end_pos : Lexing.position;
}

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool

val make : Lexing.position -> Lexing.position -> t
