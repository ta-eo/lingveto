%{
  open Ast

  let mk desc s e = { desc; loc = Loc.make s e }
%}

%token <int64> INT

%token TRUE FALSE

%token <string> IDENT

%token FUN ARROW

%token LET REC IN

%token IF THEN ELSE

%token EQ

%token LT

%token PLUS MINUS

%token STAR

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
      { mk (Let (Id.of_string x, e1, e2)) $startpos $endpos }
  
  | LET x=IDENT COLON tau=ty EQ e1=expr IN e2= expr
      { let e1' = mk (Ann (e1, tau)) $startpos(e1) $endpos(e1) in
        mk (Let (Id.of_string x, e1', e2)) $startpos $endpos }
  
  | LET REC f=IDENT EQ FUN p=param ps=list(param) ARROW body=expr IN rest=expr
      { let (x, tau) = p in
        let body' = List.fold_right
          (fun (y, tau) acc -> mk (Fun (y, tau, acc)) $startpos(ps) $endpos(body))
          ps body in
        mk (Letrec (Id.of_string f, x, tau, body', rest)) $startpos $endpos }

  | FUN ps=nonempty_list(param) ARROW body=expr
      { List.fold_right
          (fun (x, tau) acc -> mk (Fun (x, tau, acc)) $startpos $endpos)
          ps body }
  
  | IF e1=expr THEN e2=expr ELSE e3=expr
      { mk (If (e1, e2, e3)) $startpos $endpos }
  
  | e=cmp
      { e }


cmp:
  | l=term EQ r=term
      { mk (Prim2 (Eq, l, r)) $startpos $endpos }

  | l=term LT r=term
      { mk (Prim2 (Lt, l, r)) $startpos $endpos }

  | e=term
      { e }

term:
  | l=term PLUS r=factor
      { mk (Prim2 (Add, l, r)) $startpos $endpos }
  
  | l=term MINUS r=factor
      { mk (Prim2 (Sub, l, r)) $startpos $endpos }  
 
  | e=factor
      { e }
  
factor:
  | l=factor STAR r=unary
      { mk (Prim2 (Mul, l, r)) $startpos $endpos }
  
  | e=unary
      { e }

unary:
  | MINUS e=unary
      { mk (Neg e) $startpos $endpos }
  
  | e=app
      { e }

app:
  | func=app arg=atom
      { mk (App (func, arg)) $startpos $endpos }
  
  | e=atom
      { e }
  
atom:
  | n=INT
      { mk (Int n) $startpos $endpos }
  
  | TRUE
      { mk (Bool true) $startpos $endpos }
  
  | FALSE
      { mk (Bool false) $startpos $endpos }

  | x=IDENT
      { mk (Var (Id.of_string x)) $startpos $endpos }
  
  | LPAREN e=expr COLON tau=ty RPAREN
      { mk (Ann (e, tau)) $startpos $endpos }

  | LPAREN e=expr RPAREN
      { e }
  
  | BEGIN e=expr END
      { e }

ty:
  | t1=ty_atom ARROW t2=ty
      { mk (TFun (t1, t2)) $startpos $endpos }
  
  | tau=ty_atom
      { tau }

ty_atom:
  | TINT
      { mk TInt $startpos $endpos }
  
  | TBOOL
      { mk TBool $startpos $endpos }
  
  | LPAREN tau=ty RPAREN
      { tau }

param:
  | x=IDENT
      { (Id.of_string x, None) }
  
  | LPAREN x=IDENT COLON tau=ty RPAREN
      { (Id.of_string x, Some tau) }
