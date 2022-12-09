open Base
open Stdio

let day = 8

let tests () =
  match
    Forest.of_string_list [ "30373"; "25512"; "65332"; "33549"; "35390" ]
  with
  | Ok forest ->
      printf
        "The tree with the highest scenic score is %d, and it should be 8.\n"
        (Forest.highest_scenic_score forest);
      printf "trees visible left should be 2 and is %d\n"
        (Forest.trees_visible_left forest 3 2)
  | Error _ -> printf "The test failed to run.\n"

let run () =
  tests ();
  let input = In_channel.read_lines (Utils.input_path_for_day 8) in
  match Forest.of_string_list input with
  | Ok forest ->
      printf "There are %d visible trees in the forest.\n"
        (Forest.count_visible forest);
      printf "The tree with the highest scenic score is %d.\n"
        (Forest.highest_scenic_score forest)
  | Error err -> print_endline (Error.to_string_hum err)
