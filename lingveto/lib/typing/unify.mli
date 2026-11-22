exception Occurs
exception Mismatch of Type.t * Type.t

val occurs : Type.id -> Type.level -> Type.t -> unit

val f : Type.t -> Type.t -> unit