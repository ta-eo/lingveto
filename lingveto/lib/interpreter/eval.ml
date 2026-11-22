open Syntax.Ast

open Value

let rec f env e =
  match e.desc with
  | Int n ->
      VInt n
  
  | Bool b ->
      VBool b
  
  | Var x ->
      begin match Env.lookup x env with
      | Some v -> v
      | None -> assert false
      end
  
  | Fun (x, _, e1) ->
      VFun { self = None; param = x; body = e1; env }
  
  | App (func, arg) ->
      let fv = f env func in
      let av = f env arg in
      apply fv av
  
  | Let (x, e1, e2) ->
      let v = f env e1 in
      f (Env.extend x v env) e2
  
  | Letrec (g, x, _, e1, e2) ->
      let clos = VFun { self = Some g; param = x; body = e1; env } in
      f (Env.extend g clos env) e2
    
  | If (cond, e1, e2) ->
      begin match f env cond with
      | VBool true -> f env e1
      | VBool false -> f env e2
      | _ -> assert false
      end

  | Prim2 (op, e1, e2) ->
      let v1 = f env e1 in
      let v2 = f env e2 in
      begin match op, v1, v2 with
      | Add, VInt a, VInt b -> VInt (Int64.add a b)
      | Lt, VInt a, VInt b -> VBool (Int64.compare a b < 0)
      | _ -> assert false
      end
  
  | Ann (e1, _) -> f env e1

and apply fv av =
  match fv with
  | VFun clos ->
      let env =
        match clos.self with
        | Some g -> Env.extend g fv clos.env
        | None -> clos.env
      in
      f (Env.extend clos.param av env) clos.body
  | _ ->
      assert false