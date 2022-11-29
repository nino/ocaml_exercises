open ContainersLabels
module StringMap : Map.S

type t = {
  capacity : int;
  durability : int;
  flavor : int;
  texture : int;
  calories : int;
}

type cupboard = t StringMap.t

val empty_cupboard : cupboard
val cupboard_add : cupboard -> string -> t -> cupboard
val cupboard_find_exn : cupboard -> string -> t
val ingredients_in_cupboard : cupboard -> string list
val sexp_of_t : t -> Sexp.t
