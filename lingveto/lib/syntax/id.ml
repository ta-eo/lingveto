(* lib/syntax/id.ml *)

type t = string
[@@deriving show { with_path = false }, eq]

let of_string s = s

let to_string s = s
