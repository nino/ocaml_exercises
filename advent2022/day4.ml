open Base
open Stdio

let day = 4

module Range = struct
  let create start stop =
    if start > stop then failwith "start needs to be <= stop" else (start, stop)

  let contains (start1, stop1) (start2, stop2) =
    start1 <= start2 && stop1 >= stop2
end

let run () =
  let input = In_channel.read_lines (Utils.input_path_for_day 4) in
  let pairs =
    List.map input ~f:(fun line ->
        let nums = String.split_on_chars ~on:[ ','; '-' ] line in
        match nums with
        | [ start1; stop1; start2; stop2 ] ->
            ( Range.create (Int.of_string start1) (Int.of_string stop1),
              Range.create (Int.of_string start2) (Int.of_string stop2) )
        | _ -> failwith "Invalid line")
  in
  let fully_contained_count =
    List.count pairs ~f:(fun (a, b) -> Range.contains a b || Range.contains b a)
  in
  printf "There are %d pairs where one fully contains the other.\n"
    fully_contained_count;
  ()
