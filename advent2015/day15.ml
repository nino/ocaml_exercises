open ContainersLabels
module StringMap = Map.Make (String)

let parse_and_add cupboard line =
  let pattern =
    Re.compile
    @@ Re.Pcre.re
         {|(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)|}
  in
  match Re.exec pattern line |> Re.Group.all with
  | [| _everything; name; capacity; durability; flavor; texture; calories |] ->
      Ingredient.cupboard_add cupboard name
        {
          Ingredient.capacity = int_of_string capacity;
          durability = int_of_string durability;
          flavor = int_of_string flavor;
          texture = int_of_string texture;
          calories = int_of_string calories;
        }
  | _ -> cupboard

let run () =
  Utils.greet 15;
  let lines = IO.(with_in (Utils.input_path_for_day 15) read_lines_l) in
  let cupboard =
    List.fold_left lines ~init:Ingredient.empty_cupboard ~f:parse_and_add
  in
  Printf.printf "The ingredients are:\n";
  let optimal_recipe, score = Recipe.get_optimal_recipe cupboard in
  Printf.printf "The optimal recipe is:\n%s\n\nand it has the score %d.\n"
    (Sexp.to_string (Recipe.sexp_of_t optimal_recipe))
    score;
  ()
