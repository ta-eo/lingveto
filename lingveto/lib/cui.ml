type error =
  | Parse of Syntax.Diagnostics.t
  | Type of Typing.Type_error.t

let eval_string ?fname s =
  match Syntax.Parse.parse_string ?fname s with
  | Error d -> Error (Parse d)
  | Ok ast ->
      match Typing.Infer.run ast with
      | Error e -> Error (Type e)
      | Ok ty -> Ok (Interpreter.Eval.f Env.empty ast, ty)
