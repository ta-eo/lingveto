open Syntax.Ast

let unify t1 t2 (at : _ node) =
  try Unify.unify t1 t2 with
  | Unify.Occurs ->
      Type_error.raise_at at.loc Type_error.Occurs
  | Unify.Mismatch (a, b) ->
      Type_error.raise_at at.loc (Type_error.Mismatch (a, b))

let rec of_ast_ty (t : ty) : Ty.t =
  match t.desc with
  | TInt -> Ty.TInt
  | TBool -> Ty.TBool
  | TFun (a, b) -> Ty.TFun (of_ast_ty a, of_ast_ty b)

let rec infer gamma (e : expr) : Ty.t =
  match e.desc with
  | Int _ -> Ty.TInt

  | Bool _ -> Ty.TBool

  | Var x ->
      begin match Env.lookup x gamma with
      | Some sigma -> Instantiate.f sigma
      | None -> Type_error.raise_at e.loc (Type_error.Unbound_var x)
      end

  | Fun (x, ann, body) ->
      let tx =
        match ann with
        | Some t -> of_ast_ty t
        | None -> Ty.fresh_tvar ()
      in
      let tb = infer (Env.extend x tx gamma) body in
      Ty.TFun (tx, tb)

  | App (fn, arg) ->
      let tf = infer gamma fn in
      let ta = infer gamma arg in
      let tr = Ty.fresh_tvar () in
      unify tf (Ty.TFun (ta, tr)) e;
      tr

  | Let (x, e1, e2) ->
      Ty.enter_level ();
      let t1 = infer gamma e1 in
      Ty.leave_level ();
      infer (Env.extend x (Generalise.f t1) gamma) e2

  | Letrec (g, x, ann, body, rest) ->
      Ty.enter_level ();
      let tg = Ty.fresh_tvar () in
      let tx =
        match ann with
        | Some t -> of_ast_ty t
        | None -> Ty.fresh_tvar ()
      in
      let tr = Ty.fresh_tvar () in
      unify tg (Ty.TFun (tx, tr)) e;
      let tb = infer (Env.extend x tx (Env.extend g tg gamma)) body in
      unify tr tb body;
      Ty.leave_level ();
      infer (Env.extend g (Generalise.f tg) gamma) rest

  | If (c, e1, e2) ->
      unify (infer gamma c) Ty.TBool c;
      let t1 = infer gamma e1 in
      unify t1 (infer gamma e2) e2;
      t1

  | Prim2 (op, e1, e2) ->
      unify (infer gamma e1) Ty.TInt e1;
      unify (infer gamma e2) Ty.TInt e2;
      begin match op with
      | Add
      | Sub 
      | Mul -> Ty.TInt
      | Eq
      | Lt -> Ty.TBool
      end

  | Ann (e1, ty) ->
      let t = of_ast_ty ty in
      unify (infer gamma e1) t e1;
      t

let run e =
  Ty.reset ();
  try
    Ty.enter_level ();
    let t = infer Env.empty e in
    Ty.leave_level ();
    Ok (Generalise.f t)
  with Type_error.Error err -> Error err
