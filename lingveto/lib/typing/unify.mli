exception Occurs
exception Mismatch of Ty.t * Ty.t

val occurs : Ty.id -> Ty.level -> Ty.t -> unit

val unify : Ty.t -> Ty.t -> unit