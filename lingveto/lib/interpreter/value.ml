open Syntax

type t =
  | VInt of int64
  | VBool of bool
  | VFun of { 
      self : Id.t option; 
      param : Id.t;
      body : Ast.expr;
      env : t Env.t;
    }

let to_string = function
  | VInt n -> Int64.to_string n
  | VBool b -> if b then "vera" else "falsa"
  | VFun _ -> "<funkcio>"



