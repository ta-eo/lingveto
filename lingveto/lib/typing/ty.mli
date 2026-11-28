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

val repr : t -> t

val current_level : level ref

val enter_level : unit -> unit

val leave_level : unit -> unit

val fresh_type_var : unit -> t

val reset : unit -> unit

val to_string : t -> string
