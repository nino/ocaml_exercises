open Base
open Stdio

let day = 6

let find_offset str =
  let len = String.length str in
  let num_distinct_chars = 4 in
  let indices = List.range num_distinct_chars (len + 1) in
  List.find_exn indices ~f:(fun idx ->
      let chars = Hash_set.create (module Char) in
      for i = 1 to num_distinct_chars do
        Hash_set.add chars str.[idx - i]
      done;
      Hash_set.length chars = num_distinct_chars)

let run () =
  let input = In_channel.read_all (Utils.input_path_for_day 6) in
  let offset = find_offset input in
  printf "The offset is %d.\n" offset;
  ()
