(* lib/syntax/diagnostics.ml *)

type kind =
  | Unexpected_char of string
  | Int_literal_out_of_range of string
  | Syntax_error of string

type t = { kind : kind; loc : Loc.t }

exception Error of t

let raise_at loc kind = raise (Error { kind; loc })


(* esperantigos *)
let message = function
  | Unexpected_char c -> Printf.sprintf "unexpected character '%s'" c
  | Int_literal_out_of_range s ->
      Printf.sprintf "integer literal out of range: %s" s
  | Syntax_error "" -> "syntax error"
  | Syntax_error s -> Printf.sprintf "syntax error near '%s'" s

let pp fmt { kind; loc } =
  Format.fprintf fmt "%a: error: %s" Loc.pp loc (message kind)
