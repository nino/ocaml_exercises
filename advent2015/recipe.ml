open ContainersLabels
module StringMap = Map.Make (String)

type t = int StringMap.t

let empty_recipe = StringMap.empty

let sexp_of_t t =
  Sexp.of_record (StringMap.to_list (StringMap.map Sexp.of_int t))

let rec get_all_recipes ?(length = 100) (ingredients : string list) : t Seq.t =
  match length with
  | 0 -> Seq.empty
  | 1 ->
      Seq.of_list
        (List.map ingredients ~f:(fun name -> StringMap.add name 1 empty_recipe))
  | _ -> (
      match ingredients with
      | [ single_ingredient ] ->
          Seq.return (StringMap.add single_ingredient length StringMap.empty)
      | first_ingred :: other_ingreds ->
          (* Generate a sub-recipe for every possible number of spoons of the
             first recipe used. *)
          let spoons_of_first_ingred = Seq.range 0 length in
          let recipes =
            Seq.flat_map
              (fun num_spoons ->
                let remaining_spoons = length - num_spoons in
                let sub_recipes =
                  get_all_recipes ~length:remaining_spoons other_ingreds
                in
                Seq.map (StringMap.add first_ingred num_spoons) sub_recipes)
              spoons_of_first_ingred
          in
          recipes
      | _ -> Seq.empty)

let score ~ingredients recipe =
  let attribute_total get_attr =
    let raw_score =
      StringMap.fold
        (fun ingredient_name amount sum ->
          let ingredient =
            Ingredient.cupboard_find_exn ingredients ingredient_name
          in
          sum + (amount * get_attr ingredient))
        recipe 0
    in
    Int.max raw_score 0 (* Don't let an attribute's score go below 0 *)
  in
  let capacity = attribute_total (fun i -> i.Ingredient.capacity) in
  let durability = attribute_total (fun i -> i.Ingredient.durability) in
  let flavor = attribute_total (fun i -> i.Ingredient.flavor) in
  let texture = attribute_total (fun i -> i.Ingredient.texture) in
  capacity * durability * flavor * texture

let get_optimal_recipe ingredients =
  let ingredient_names = Ingredient.ingredients_in_cupboard ingredients in
  let all_recipes = get_all_recipes ingredient_names in
  Seq.fold
    (fun (best_recipe, best_score) next_recipe ->
      let next_score = score ~ingredients next_recipe in
      if next_score > best_score then (next_recipe, next_score)
      else (best_recipe, best_score))
    (StringMap.empty, 0) all_recipes
