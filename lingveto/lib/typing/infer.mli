val infer : Ty.t Env.t -> Syntax.Ast.expr -> Typed_ast.expr
val run : Syntax.Ast.t -> (Typed_ast.expr, Type_error.t) result
