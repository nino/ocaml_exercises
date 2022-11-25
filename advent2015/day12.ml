open Containers

let run () =
  Utils.greet 12;

  let input_path = Utils.input_path_for_day 12 in
  let input = IO.(with_in input_path read_all) in
  let pattern = Re.compile @@ Re.Pcre.re {|-?\d+|} in
  let sum =
    Re.all pattern input
    |> CCList.map (fun g -> Re.Group.get g 0 |> int_of_string)
    |> CCList.fold_left ( + ) 0
  in
  Printf.printf "The sum is %d.\n%!" sum
