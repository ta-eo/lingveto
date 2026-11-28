type level = int
[@@deriving show { with_path = false }, eq]

type id = int
[@@deriving show { with_path = false }, eq]

type t =
  | TInt
  | TBool
  | TVar of type_var ref
  | TFun of t * t
  | TGen of id

and type_var =
  | Unbound of id * level
  | Link of t
[@@deriving show { with_path = false }, eq]

let rec repr = function
  | TVar ({ contents = Link t } as r) ->
      let t' = repr t in
      r := Link t';
      t'
  | t -> t

let current_tvar = ref 0
let current_level = ref 0
let enter_level () = incr current_level
let leave_level () = decr current_level

let fresh_type_var () =
  incr current_tvar;
  TVar (ref (Unbound (!current_tvar, !current_level)))

let reset () =
  current_tvar := 0;
  current_level := 0

let tgen_name n =
  let letter = String.make 1 (Char.chr (Char.code 'a' + n mod 26)) in
  if n < 26 then "'" ^ letter else "'" ^ letter ^ string_of_int (n / 26)

let rec to_string_prec p t =
match repr t with
| TInt -> "int"
| TBool -> "bool"
| TVar { contents = Unbound (id, _) } -> Printf.sprintf "'_%d" id
| TVar { contents = Link _ } -> assert false  (* repr で除去済み *)
| TGen n -> tgen_name n
| TFun (a, b) ->
    let s = to_string_prec 1 a ^ " -> " ^ to_string_prec 0 b in
    if p > 0 then "(" ^ s ^ ")" else s

let to_string t = to_string_prec 0 t
