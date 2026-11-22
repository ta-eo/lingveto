val f : Type.t Env.t -> Syntax.Ast.expr -> Type.t

val run : Syntax.Ast.t -> (Type.t, Type_error.t) result
