module Ast = Syntax.Ast
module Tast = Typed_ast

let unify t1 t2 (at : _ Ast.node) =
  try Unify.f t1 t2 with
  | Unify.Occurs ->
      Type_error.raise_at at.loc Type_error.Occurs
  | Unify.Mismatch (a, b) ->
      Type_error.raise_at at.loc (Type_error.Mismatch (a, b))

let rec of_type_ann (t : Ast.type_ann) : Ty.t =
  match t.desc with
  | TAInt -> Ty.TInt
  | TABool -> Ty.TBool
  | TAFun (a, b) -> Ty.TFun (of_type_ann a, of_type_ann b)

let mk e desc ty = { Tast.desc; ty; loc = e.Ast.loc }

let rec check_and_infer gamma e expected =
  let te = infer gamma e in
  unify te.ty expected e;
  te

and infer gamma (e : Ast.expr) : Tast.expr =
  match e.desc with
  | Int n -> mk e (Tast.Int n) Ty.TInt

  | Bool b -> mk e (Tast.Bool b) Ty.TBool

  | Neg e1 ->
      let t1 = check_and_infer gamma e Ty.TInt in
      mk e (Tast.Neg t1) Ty.TInt

  | Var x ->
      begin match Env.lookup x gamma with
      | Some sigma -> mk e (Tast.Var x) (Instantiate.f sigma)
      | None -> Type_error.raise_at e.loc (Type_error.Unbound_var x)
      end

  | Fun (x, ann, body) ->
      let tx =
        match ann with
        | Some t -> of_type_ann t
        | None -> Ty.fresh_type_var ()
      in
      let tb = infer (Env.extend x tx gamma) body in
      mk e (Tast.Fun (x, tx, tb)) (Ty.TFun (tx, tb.ty))

  | App (fn, arg) ->
      let tf = infer gamma fn in
      let ta = infer gamma arg in
      let tr = Ty.fresh_type_var () in
      unify tf.ty (Ty.TFun (ta.ty, tr)) e;
      mk e (Tast.App (tf, ta)) tr

  | Let (x, e1, e2) ->
      Ty.enter_level ();
      let t1 = infer gamma e1 in
      Ty.leave_level ();
      let t2 = infer (Env.extend x (Generalise.f t1.ty) gamma) e2 in
      mk e (Tast.Let (x, t1, t2)) t2.ty

  | Letrec (g, x, ann, body, rest) ->
      Ty.enter_level ();
      let tg = Ty.fresh_type_var () in
      let tx = match ann with 
        | Some t -> of_type_ann t 
        | None -> Ty.fresh_type_var () 
      in
      let tr = Ty.fresh_type_var () in
      unify tg (Ty.TFun (tx, tr)) e;
      let tb = infer (Env.extend x tx (Env.extend g tg gamma)) body in
      unify tr tb.ty body;
      Ty.leave_level ();
      let trest = infer (Env.extend g (Generalise.f tg) gamma) rest in
      mk e (Tast.Letrec (g, x, tx, tb, trest)) trest.ty

  | If (c, e1, e2) ->
      let tc = check_and_infer gamma c Ty.TBool in
      let t1 = infer gamma e1 in
      let t2 = check_and_infer gamma e2 t1.ty in
      mk e (Tast.If (tc, t1, t2)) t1.ty

  | Prim2 (op, e1, e2) ->
      let t1 = check_and_infer gamma e1 Ty.TInt in
      let t2 = check_and_infer gamma e2 Ty.TInt in
      let ty = match op with
        | Add | Sub | Mul -> Ty.TInt
        | Eq | Lt -> Ty.TBool in
      mk e (Tast.Prim2 (op, t1, t2)) ty

  | Ann (e1, type_ann) ->
      let t = of_type_ann type_ann in
      let t1 = check_and_infer gamma e1 t in
      { t1 with loc = e.loc }

let run e : (Typed_ast.expr, Type_error.t) result =
  Ty.reset ();
  try
    Ty.enter_level ();
    let typed_e = infer Env.empty e in
    Ty.leave_level ();
    Ok (Zonk.expr typed_e)
  with
  | Type_error.Error err -> Error err
