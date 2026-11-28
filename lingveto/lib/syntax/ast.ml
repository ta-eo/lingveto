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
  | Neg of expr
  | Bool of bool
  | Var of Id.t
  | Fun of Id.t * type_ann option * expr (* fun (id : tau) -> e *)
  | App of expr * expr
  | Let of Id.t * expr * expr
  | Letrec of Id.t * Id.t * type_ann option * expr * expr (* let rec f = fun (x : tau) -> body in rest *)
  | If of expr * expr * expr
  | Prim2 of prim2 * expr * expr
  | Ann of expr * type_ann (* (e : tau) *)

and prim2 =
  | Add
  | Sub
  | Mul
  | Eq
  | Lt

and type_ann = type_ann_desc node

and type_ann_desc =
  | TAInt
  | TABool
  | TAFun of type_ann * type_ann
[@@deriving show { with_path = false }, eq]

