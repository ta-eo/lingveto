(* lib/typing/ast.ml *)

type 'a node = {
  desc : expr_desc;
  ty : Ty.t;
  loc : Syntax.Loc.t;
}

and t = expr

and expr = expr_desc node

and expr_desc =
  | Int of int64
  | Neg of expr
  | Bool of bool
  | Var of Syntax.Id.t
  | Fun of Syntax.Id.t * Ty.t * expr
  | App of expr * expr
  | Let of Syntax.Id.t * expr * expr
  | Letrec of Syntax.Id.t * Syntax.Id.t * Ty.t * expr * expr
  | If of expr * expr * expr
  | Prim2 of Syntax.Ast.prim2 * expr * expr
