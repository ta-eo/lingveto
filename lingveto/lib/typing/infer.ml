open Syntax.Ast

let unify t1 t2 (at : _ node) =
  try Unify.f t1 t2 with
  | Unify.Occurs ->
      Type_error.raise_at at.loc Type_error.Occurs
  | Unify.Mismatch (a, b) ->
      Type_error.raise_at at.loc (Type_error.Mismatch (a, b))

let rec of_ast_ty (t : ty) : Type.t =
  match t.desc with
  | TInt -> Type.TInt
  | TBool -> Type.TBool
  | TFun (a, b) -> Type.TFun (of_ast_ty a, of_ast_ty b)

let rec f gamma (e : expr) : Type.t =
  match e.desc with
  | Int _ -> Type.TInt

  | Bool _ -> Type.TBool

  | Var x ->
      begin match Env.lookup x gamma with
      | Some sigma -> Instantiate.f sigma
      | None -> Type_error.raise_at e.loc (Type_error.Unbound_var x)
      end

  | Fun (x, ann, body) ->
      let tx =
        match ann with
        | Some t -> of_ast_ty t
        | None -> Type.fresh_tvar ()
      in
      let tb = f (Env.extend x tx gamma) body in
      Type.TFun (tx, tb)

  | App (fn, arg) ->
      let tf = f gamma fn in
      let ta = f gamma arg in
      let tr = Type.fresh_tvar () in
      unify tf (Type.TFun (ta, tr)) e;
      tr

  | Let (x, e1, e2) ->
      Type.enter_level ();
      let t1 = f gamma e1 in
      Type.leave_level ();
      f (Env.extend x (Generalise.f t1) gamma) e2

  | Letrec (g, x, ann, body, rest) ->
      Type.enter_level ();
      let tg = Type.fresh_tvar () in
      let tx =
        match ann with
        | Some t -> of_ast_ty t
        | None -> Type.fresh_tvar ()
      in
      let tr = Type.fresh_tvar () in
      unify tg (Type.TFun (tx, tr)) e;
      let tb = f (Env.extend x tx (Env.extend g tg gamma)) body in
      unify tr tb body;
      Type.leave_level ();
      f (Env.extend g (Generalise.f tg) gamma) rest

  | If (c, e1, e2) ->
      unify (f gamma c) Type.TBool c;
      let t1 = f gamma e1 in
      unify t1 (f gamma e2) e2;
      t1

  | Prim2 (op, e1, e2) ->
      unify (f gamma e1) Type.TInt e1;
      unify (f gamma e2) Type.TInt e2;
      begin match op with
      | Add -> Type.TInt
      | Lt -> Type.TBool
      end

  | Ann (e1, ty) ->
      let t = of_ast_ty ty in
      unify (f gamma e1) t e1;
      t

let run e =
  Type.reset ();
  try
    Type.enter_level ();
    let t = f Env.empty e in
    Type.leave_level ();
    Ok (Generalise.f t)
  with Type_error.Error err -> Error err
