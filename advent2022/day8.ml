open Base
open Stdio

let day = 8

let run () =
  let input = In_channel.read_lines (Utils.input_path_for_day 8) in
  match Forest.of_string_list input with
  | Ok forest ->
      printf "There are %d visible trees in the forest.\n"
        (Forest.count_visible forest)
  | Error err -> print_endline (Error.to_string_hum err)
