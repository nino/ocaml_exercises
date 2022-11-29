open ContainersLabels
module StringMap : Map.S

type t

val get_optimal_recipe : Ingredient.cupboard -> t * int
(** [get_optimal_recipe ingredients] returns the optimal recipe and its score,
    for a given map of ingredient names to their attributes. *)

val sexp_of_t : t -> Sexp.t
