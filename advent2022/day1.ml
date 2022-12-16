open Base

let day = 1
let parse_int_or_zero str = try Int.of_string str with _ -> 0

let run () =
  let open Stdio in
  let input = In_channel.read_all (Utils.input_path_for_day 1) in
  let elves =
    String.Search_pattern.split_on (String.Search_pattern.create "\n\n") input
    |> List.map ~f:(fun group ->
           String.split ~on:'\n' group |> List.map ~f:parse_int_or_zero)
  in
  let elves = elves |> List.map ~f:(List.fold ~init:0 ~f:( + )) in
  let biggest_elf_storage = elves |> List.fold ~init:0 ~f:Int.max in
  printf "The elf with the most calories has %d calories.\n" biggest_elf_storage;
  let sorted_elves = List.sort elves ~compare:(fun a b -> -Int.compare a b) in
  let biggest_3_elves_together =
    List.take sorted_elves 3 |> List.fold ~init:0 ~f:( + )
  in
  printf "The biggest 3 elves have %d calories together.\n"
    biggest_3_elves_together;
  ()
