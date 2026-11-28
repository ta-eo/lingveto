let rec ty t =
  match Ty.repr t with
  | Ty.TFun (a, b) -> Ty.TFun (ty a, ty b)
  | t -> t

let rec expr (e : Typed_ast.expr) : Typed_ast.expr =
  let desc =
    match e.desc with
    | (Int _ | Bool _ | Var _) as d -> d
    | Neg a -> Neg (expr a)
    | Fun (x, t, b) -> Fun (x, ty t, expr b)
    | App (f, a) -> App (expr f, expr a)
    | Let (x, a, b) -> Let (x, expr a, expr b)
    | Letrec (g, x, t, a, b) -> Letrec (g, x, ty t, expr a, expr b)
    | If (c, a, b) -> If (expr c, expr a, expr b)
    | Prim2 (op, a, b) -> Prim2 (op, expr a, expr b)
  in
  { e with desc; ty = ty e.ty }
