type t

val create : ?sandbox_root:string -> unit -> t
val define : t -> string -> string -> unit
val expand : t -> string -> (string, string) result
