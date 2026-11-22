(* lib/syntax/id.mli *)

type t
[@@deriving show { with_path = false }, eq]

val of_string : string -> t

val to_string : t -> string
