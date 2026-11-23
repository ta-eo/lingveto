(* lib/syntax/ast.ml *)

type 'a node = {
  desc : 'a; 
  loc : Loc.t [@opaque] [@equal fun _ _ -> true]
}
[@@deriving show { with_path = false }, eq]

type t = expr

and expr = expr_desc node

and expr_desc =
  | Int of int64
  | Bool of bool
  | Var of Id.t
  | Fun of Id.t * ty option * expr (* fun (id : tau) -> e *)
  | App of expr * expr
  | Let of Id.t * expr * expr
  | Letrec of Id.t * Id.t * ty option * expr * expr (* let rec f = fun (x : tau) -> body in rest *)
  | If of expr * expr * expr
  | Prim2 of prim2 * expr * expr
  | Ann of expr * ty (* (e : tau) *)

and prim2 =
  | Add
  | Sub
  | Lt

and ty = ty_desc node

and ty_desc =
  | TInt
  | TBool
  | TFun of ty * ty
[@@deriving show { with_path = false }, eq]

