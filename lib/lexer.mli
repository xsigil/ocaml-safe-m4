type token_type =
  | TokenEOF
  | TokenIdent of string
  | TokenQuoted of string
  | TokenComment of string
  | TokenOther of char

type t

val create : unit -> t
val push_input : t -> string -> unit
val read_char : t -> char option
val unread_char : t -> char -> unit
val match_prefix : t -> string -> bool
val read_until_matching : t -> string -> string -> string
val next_token : t -> token_type
val set_quotes : t -> string -> string -> unit
val set_comments : t -> string -> string -> unit
val get_quotes : t -> string * string
