open Base

type t

val of_string_list : string list -> (t, Error.t) Result.t
val count_visible : t -> int
