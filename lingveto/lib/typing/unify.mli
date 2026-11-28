exception Occurs
exception Mismatch of Ty.t * Ty.t

val occurs : Ty.id -> Ty.level -> Ty.t -> unit

val f : Ty.t -> Ty.t -> unit