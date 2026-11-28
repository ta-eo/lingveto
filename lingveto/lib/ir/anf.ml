(* lib/ir/anf.ml *)

type atom =
  | Int of int64
  | Bool of bool
  | Var of Syntax.Id.t
  [@@deriving show { with_path = false }]

type binder = Syntax.Id.t * Typing.Ty.t
  [@@deriving show { with_path = false }]

type cexp =
  | Atom of atom
  | Neg of atom
  | Prim2 of Syntax.Ast.prim2 * atom * atom
  | App of atom * atom
  | Fun of binder * expr
  | If of atom * expr * expr

and expr =
  | Let of binder * cexp * expr
  | Letrec of binder * binder * expr * expr
  | Comp of cexp
  [@@deriving show { with_path = false }]
