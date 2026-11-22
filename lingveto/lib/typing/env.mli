type 'a t

val empty : 'a t

val extend : Syntax.Id.t -> 'a -> 'a t -> 'a t

val lookup : Syntax.Id.t -> 'a t -> 'a option
