open Base

type t

val of_string_list : string list -> (t, Error.t) Result.t
val count_visible : t -> int
val highest_scenic_score : t -> int
val trees_visible_left : t -> int -> int -> int
val trees_visible_right : t -> int -> int -> int
val trees_visible_top : t -> int -> int -> int
val trees_visible_bottom : t -> int -> int -> int
