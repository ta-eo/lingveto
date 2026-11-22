%{
  open Ast

  let t desc s e = { desc; loc = Loc.make s e }
%}

%token <int64> INT

%token TRUE FALSE

%token <string> IDENT

%token FUN ARROW

%token LET REC IN

%token IF THEN ELSE

%token EQ

%token LT

%token PLUS

%token LPAREN RPAREN BEGIN END

%token TINT TBOOL

%token COLON

%token EOF

%start <Ast.t> main

%%

main:
  | e=expr EOF
      { e }

expr:
  | LET x=IDENT EQ e1=expr IN e2=expr
      { t (Let (Id.of_string x, e1, e2)) $startpos $endpos }
  
  | LET x=IDENT COLON tau=ty EQ e1=expr IN e2= expr
      { let e1' = t (Ann (e1, tau)) $startpos(e1) $endpos(e1) in
        t (Let (Id.of_string x, e1', e2)) $startpos $endpos }
  
  | LET REC f=IDENT EQ FUN p=param ps=list(param) ARROW body=expr IN rest=expr
      { let (x, tau) = p in
        let body' = List.fold_right
          (fun (y, tau) acc -> t (Fun (y, tau, acc)) $startpos(ps) $endpos(body))
          ps body in
        t (Letrec (Id.of_string f, x, tau, body', rest)) $startpos $endpos }

  | FUN ps=nonempty_list(param) ARROW body=expr
      { List.fold_right
          (fun (x, tau) acc -> t (Fun (x, tau, acc)) $startpos $endpos)
          ps body }
  
  | IF e1=expr THEN e2=expr ELSE e3=expr
      { t (If (e1, e2, e3)) $startpos $endpos }
  
  | e=binop
      { e }

binop:
  | l=binop PLUS r=app
      { t (Prim2 (Add, l, r)) $startpos $endpos }
  
  | l=binop LT r=app
      { t (Prim2 (Lt, l, r)) $startpos $endpos }

  | e=app
      { e }

app:
  | func=app arg=atom
      { t (App (func, arg)) $startpos $endpos }
  
  | e=atom
      { e }
  
atom:
  | n=INT
      { t (Int n) $startpos $endpos }
  
  | TRUE
      { t (Bool true) $startpos $endpos }
  
  | FALSE
      { t (Bool false) $startpos $endpos }

  | x=IDENT
      { t (Var (Id.of_string x)) $startpos $endpos }
  
  | LPAREN e=expr COLON tau=ty RPAREN
      { t (Ann (e, tau)) $startpos $endpos }

  | LPAREN e=expr RPAREN
      { e }
  
  | BEGIN e=expr END
      { e }

ty:
  | t1=ty_atom ARROW t2=ty
      { t (TFun (t1, t2)) $startpos $endpos }
  
  | t=ty_atom
      { t }

ty_atom:
  | TINT
      { t TInt $startpos $endpos }
  
  | TBOOL
      { t TBool $startpos $endpos }
  
  | LPAREN t=ty RPAREN
      { t }

param:
  | x=IDENT
      { (Id.of_string x, None) }
  
  | LPAREN x=IDENT COLON tau=ty RPAREN
      { (Id.of_string x, Some tau) }
