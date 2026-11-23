(* lib/typing/Ty_error.ml *)

type kind =
  | Mismatch of Ty.t * Ty.t
  | Occurs
  | Unbound_var of Syntax.Id.t

type t = { kind : kind; loc : Syntax.Loc.t }

exception Error of t

let raise_at loc kind = raise (Error { kind; loc })

let message = function
  | Mismatch (a, b) ->
      Printf.sprintf "Type mismatch: cannot unify %s with %s"
        (Ty.to_string a) (Ty.to_string b)
  | Occurs -> "occurs check failed: cannot construct an infinite Type"
  | Unbound_var x ->
      Printf.sprintf "unbound variable '%s'" (Syntax.Id.to_string x)

let pp fmt { kind; loc } =
  Format.fprintf fmt "%a: error: %s" Syntax.Loc.pp loc (message kind)
