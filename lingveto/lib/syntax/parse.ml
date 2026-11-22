(* lib/syntax/parse.ml *)

let parse_string ?(fname = "<string>") s : (Ast.t, Diagnostics.t) result =
  let buf = Sedlexing.Utf8.from_string s in
  Sedlexing.set_filename buf fname;

  let supplier () = Lexer.token_with_pos buf in
  let parse = MenhirLib.Convert.Simplified.traditional2revised Parser.main in
  
  try Ok (parse supplier) with
  | Diagnostics.Error d -> 
      Error d
  | Parser.Error ->
      let s, e = Sedlexing.lexing_positions buf in
      Error 
        { kind = Syntax_error (Sedlexing.Utf8.lexeme buf);
          loc = Loc.make s e }