val infer : Ty.t Env.t -> Syntax.Ast.expr -> Ty.t

val run : Syntax.Ast.t -> (Ty.t, Type_error.t) result
