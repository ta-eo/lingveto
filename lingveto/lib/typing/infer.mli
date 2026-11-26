val infer : Ty.t Env.t -> Syntax.Ast.expr -> Ast.expr
val run : Syntax.Ast.t -> (Ast.expr, Type_error.t) result
