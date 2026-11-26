open Syntax.Ast

module Ta = Ast

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

let mk e desc ty = { Ta.desc; ty; loc = e.loc }

let rec check gamma e expected =
  let te = infer gamma e in
  unify te.ty expected e;
  te

and infer gamma (e : expr) : Ta.expr =
  match e.desc with
  | Int n -> mk e (Ta.Int n) Ty.TInt

  | Bool b -> mk e (Ta.Bool b) Ty.TBool

  | Neg e1 ->
      let t1 = check gamma e Ty.TInt in
      mk e (Ta.Neg t1) Ty.TInt

  | Var x ->
      begin match Env.lookup x gamma with
      | Some sigma -> mk e (Ta.Var x) (Instantiate.f sigma)
      | None -> Type_error.raise_at e.loc (Type_error.Unbound_var x)
      end

  | Fun (x, ann, body) ->
      let tx =
        match ann with
        | Some t -> of_ast_ty t
        | None -> Ty.fresh_tvar ()
      in
      let tb = infer (Env.extend x tx gamma) body in
      mk e (Ta.Fun (x, tx, tb)) (Ty.TFun (tx, tb.ty))

  | App (fn, arg) ->
      let tf = infer gamma fn in
      let ta = infer gamma arg in
      let tr = Ty.fresh_tvar () in
      unify tf.ty (Ty.TFun (ta.ty, tr)) e;
      mk e (Ta.App (tf, ta)) tr

  | Let (x, e1, e2) ->
      Ty.enter_level ();
      let t1 = infer gamma e1 in
      Ty.leave_level ();
      let t2 = infer (Env.extend x (Generalise.f t1.ty) gamma) e2 in
      mk e (Ta.Let (x, t1, t2)) t2.ty

  | Letrec (g, x, ann, body, rest) ->
      Ty.enter_level ();
      let tg = Ty.fresh_tvar () in
      let tx = match ann with Some t -> of_ast_ty t | None -> Ty.fresh_tvar () in
      let tr = Ty.fresh_tvar () in
      unify tg (Ty.TFun (tx, tr)) e;
      let tb = infer (Env.extend x tx (Env.extend g tg gamma)) body in
      unify tr tb.ty body;
      Ty.leave_level ();
      let trest = infer (Env.extend g (Generalise.f tg) gamma) rest in
      mk e (Ta.Letrec (g, x, tx, tb, trest)) trest.ty

  | If (c, e1, e2) ->
      let tc = check gamma c Ty.TBool in
      let t1 = infer gamma e1 in
      let t2 = check gamma e2 t1.ty in
      mk e (Ta.If (tc, t1, t2)) t1.ty

  | Prim2 (op, e1, e2) ->
      let t1 = check gamma e1 Ty.TInt in
      let t2 = check gamma e2 Ty.TInt in
      let ty = match op with
        | Add | Sub | Mul -> Ty.TInt
        | Eq | Lt -> Ty.TBool in
      mk e (Ta.Prim2 (op, t1, t2)) ty

  | Ann (e1, ty) ->
      let t = of_ast_ty ty in
      let t1 = check gamma e1 t in
      { t1 with loc = e.loc }

let run e : (Ta.expr, Type_error.t) result =
  Ty.reset ();
  try
    Ty.enter_level ();
    let t = infer Env.empty e in
    Ty.leave_level ();
    Ok (Zonk.expr t)
  with Type_error.Error err -> Error err
