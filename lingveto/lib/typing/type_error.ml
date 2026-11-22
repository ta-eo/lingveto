(* lib/typing/type_error.ml *)

type kind =
  | Mismatch of Type.t * Type.t
  | Occurs
  | Unbound_var of Syntax.Id.t

type t = { kind : kind; loc : Syntax.Loc.t }

exception Error of t

let raise_at loc kind = raise (Error { kind; loc })

let message = function
  | Mismatch (a, b) ->
      Printf.sprintf "type mismatch: cannot unify %s with %s"
        (Type.to_string a) (Type.to_string b)
  | Occurs -> "occurs check failed: cannot construct an infinite type"
  | Unbound_var x ->
      Printf.sprintf "unbound variable '%s'" (Syntax.Id.to_string x)

let pp fmt { kind; loc } =
  Format.fprintf fmt "%a: error: %s" Syntax.Loc.pp loc (message kind)
