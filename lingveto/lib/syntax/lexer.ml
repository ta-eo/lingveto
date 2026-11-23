(* lib/syntax/lexer.ml *)

open Sedlexing

open Parser

let digit = [%sedlex.regexp? '0' .. '9']

let eo_maj = [%sedlex.regexp? 0x0108 | 0x011C | 0x0124 | 0x0134 | 0x015C | 0x016C]
let eo_min = [%sedlex.regexp? 0x0109 | 0x011D | 0x0125 | 0x0135 | 0x015D | 0x016D]

let upper = [%sedlex.regexp? eo_maj | 'A' .. 'Z']
let lower = [%sedlex.regexp? eo_min | 'a' .. 'z']

let snake_case = [%sedlex.regexp? (lower | '_'), Star (lower | digit | '_' | '\'')]

let keyword_or_ident loc = function
  | "vera"   -> TRUE
  | "falsa"  -> FALSE
  | "fn"     -> FUN
  | "est"    -> LET
  | "rek"    -> REC
  | "en"     -> IN
  | "se"     -> IF
  | "tiam"   -> THEN
  | "alie"   -> ELSE
  | "ent"    -> TINT
  | "bul"    -> TBOOL
  | "komenc" -> BEGIN
  | "fin"    -> END
  | "kaze"   -> Diagnostics.raise_at loc (Diagnostics.Reserved_word "kaze")
  | "de"     -> Diagnostics.raise_at loc (Diagnostics.Reserved_word "de")
  | "tip"    -> Diagnostics.raise_at loc (Diagnostics.Reserved_word "tip")
  | name     -> IDENT name

let loc buf =
  let s, e = lexing_positions buf in
  Loc.make s e

let rec token buf =
  match%sedlex buf with
  | '\n' -> 
      Sedlexing.new_line buf; 
      token buf
  
  | Plus (Sub (white_space, '\n')) -> 
      token buf

  | "//", Star (Compl '\n') -> 
      token buf
  
  | Plus digit ->
      let s = Utf8.lexeme buf in
      begin match Int64.of_string_opt s with
      | Some n -> INT n
      | None -> Diagnostics.raise_at (loc buf) (Diagnostics.Int_literal_out_of_range s)
      end

  | snake_case ->
      keyword_or_ident (loc buf) (Utf8.lexeme buf)
  
  | '=' ->
      EQ
  
  | ':' ->
      COLON
  
  | '<' ->
      LT
  
  | "->" ->
      ARROW
  
  | '+' ->
      PLUS

  | '(' ->
      LPAREN

  | ')' ->
      RPAREN

  | eof ->
      EOF
  
  | any ->
      Diagnostics.raise_at (loc buf) (Unexpected_char (Utf8.lexeme buf))
  
  | _ ->
      assert false

let token_with_pos buf =
  let tok = token buf in
  let s, e = Sedlexing.lexing_positions buf in
  (tok, s, e)
