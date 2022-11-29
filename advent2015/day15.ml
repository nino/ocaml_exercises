open ContainersLabels
module StringMap = Map.Make (String)

module Ingredient = struct
  type t = {
    capacity : int;
    durability : int;
    flavor : int;
    texture : int;
    calories : int;
  }

  (* At some point I need to figure out how to derive this kind of stuff without
     Base *)
  let sexp_of_t t =
    Sexp.of_list
      [
        `List [ `Atom "capacity"; `Atom (string_of_int t.capacity) ];
        `List [ `Atom "durability"; `Atom (string_of_int t.durability) ];
        `List [ `Atom "flavor"; `Atom (string_of_int t.flavor) ];
        `List [ `Atom "texture"; `Atom (string_of_int t.texture) ];
        `List [ `Atom "calories"; `Atom (string_of_int t.calories) ];
      ]
end

module Recipe = struct
  type t = int StringMap.t

  let empty_recipe = StringMap.empty

  let sexp_of_t t =
    Sexp.of_record (StringMap.to_list (StringMap.map Sexp.of_int t))

  let rec get_all_recipes ?(length = 100) (ingredients : string list) : t Seq.t
      =
    match length with
    | 0 -> Seq.empty
    | 1 ->
        Seq.of_list
          (List.map ingredients ~f:(fun name ->
               StringMap.add name 1 empty_recipe))
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
            let ingredient = StringMap.find ingredient_name ingredients in
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
    let ingredient_names =
      StringMap.to_list ingredients |> List.map ~f:(fun (k, _v) -> k)
    in
    let all_recipes = get_all_recipes ingredient_names in
    Seq.fold
      (fun (best_recipe, best_score) next_recipe ->
        let next_score = score ~ingredients next_recipe in
        if next_score > best_score then (next_recipe, next_score)
        else (best_recipe, best_score))
      (StringMap.empty, 0) all_recipes
end

let parse_and_add ingredients line =
  let pattern =
    Re.compile
    @@ Re.Pcre.re
         {|(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)|}
  in
  match Re.exec pattern line |> Re.Group.all with
  | [| _everything; name; capacity; durability; flavor; texture; calories |] ->
      StringMap.add name
        {
          Ingredient.capacity = int_of_string capacity;
          durability = int_of_string durability;
          flavor = int_of_string flavor;
          texture = int_of_string texture;
          calories = int_of_string calories;
        }
        ingredients
  | _ -> ingredients

let run () =
  Utils.greet 15;
  let lines = IO.(with_in (Utils.input_path_for_day 15) read_lines_l) in
  let ingredients =
    List.fold_left lines ~init:StringMap.empty ~f:parse_and_add
  in
  Printf.printf "The ingredients are:\n";
  StringMap.iter
    (fun k v ->
      Printf.printf "%s: %s\n" k (Sexp.to_string (Ingredient.sexp_of_t v)))
    ingredients;
  let optimal_recipe, score = Recipe.get_optimal_recipe ingredients in
  Printf.printf "The optimal recipe is:\n%s\n\nand it has the score %d.\n"
    (Sexp.to_string (Recipe.sexp_of_t optimal_recipe))
    score;
  ()
