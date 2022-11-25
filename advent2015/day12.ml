open Containers

let is_value_red (_key, value) =
  match value with `String "red" -> true | _ -> false

let rec sum_numbers_but_not_red json =
  match json with
  | `Int n -> n
  | `String _ -> 0
  | `List l -> l |> List.map sum_numbers_but_not_red |> List.fold_left ( + ) 0
  | `Assoc l ->
      if List.exists is_value_red l then 0
      else
        l
        |> List.map (fun (_, v) -> sum_numbers_but_not_red v)
        |> List.fold_left ( + ) 0
  | _ -> 0

let run () =
  Utils.greet 12;

  let input_path = Utils.input_path_for_day 12 in
  let input = IO.(with_in input_path read_all) in
  let pattern = Re.compile @@ Re.Pcre.re {|-?\d+|} in
  let sum =
    Re.all pattern input
    |> List.map (fun g -> Re.Group.get g 0 |> int_of_string)
    |> List.fold_left ( + ) 0
  in
  Printf.printf "The sum is %d.\n%!" sum;

  let json = Yojson.Safe.from_string input in
  let new_count = sum_numbers_but_not_red json in
  Printf.printf "The new sum, without the reds, is %d.\n%!" new_count
