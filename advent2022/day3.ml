open Base
open Stdio

let day = 3

let priority item =
  match item with
  | 'a' .. 'z' -> Char.(1 + to_int item - to_int 'a')
  | 'A' .. 'Z' -> Char.(27 + to_int item - to_int 'A')
  | _ ->
      failwith @@ "You can only get priorities for alphabetic characters, not '"
      ^ Char.to_string item ^ "'."

module Rucksack = struct
  type t = string

  let create (input : string) : t = input

  let find_duplicate_item t =
    let bag1 = Hash_set.create (module Char) in
    let bag2 = Hash_set.create (module Char) in
    let bag_len = String.length t / 2 in
    for idx = 0 to bag_len - 1 do
      Hash_set.add bag1 t.[idx];
      Hash_set.add bag2 t.[idx + bag_len]
    done;
    let dupes = Hash_set.to_list (Hash_set.inter bag1 bag2) in
    List.hd dupes
end

let char_opt_to_str = function
  | Some c -> "Some(" ^ Char.to_string c ^ ")"
  | None -> "None"

let tests () =
  printf "%d should be 19.\n" (priority 's');
  printf "%d should be 38.\n" (priority 'L');

  printf "%s should be Some(p)\n"
    (Rucksack.create "vJrwpWtwJgWrhcsFMMfFFhFp"
    |> Rucksack.find_duplicate_item |> char_opt_to_str);
  printf "%s should be Some(L)\n"
    (Rucksack.create "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    |> Rucksack.find_duplicate_item |> char_opt_to_str);
  printf "%s should be Some(P)\n"
    (Rucksack.create "PmmdzqPrVvPwwTWBwg"
    |> Rucksack.find_duplicate_item |> char_opt_to_str);
  printf "%s should be Some(v)\n"
    (Rucksack.create "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    |> Rucksack.find_duplicate_item |> char_opt_to_str);

  let rucksacks =
    List.map ~f:Rucksack.create
      [
        "vJrwpWtwJgWrhcsFMMfFFhFp";
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL";
        "PmmdzqPrVvPwwTWBwg";
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn";
        "ttgJtRGJQctTZtZT";
        "CrZsJsPPZsGzwwsLwLmpwMDw";
      ]
  in
  let dupes = List.filter_map rucksacks ~f:Rucksack.find_duplicate_item in
  let priority_sum =
    List.fold dupes ~init:0 ~f:(fun sum item -> sum + priority item)
  in
  printf "%d should be 157.\n" priority_sum;
  ()

let run () =
  tests ();
  let input = In_channel.read_lines (Utils.input_path_for_day 3) in
  let rucksacks = List.map input ~f:Rucksack.create in
  let dupes =
    List.map rucksacks ~f:(fun rucksack ->
        match Rucksack.find_duplicate_item rucksack with
        | Some dup -> dup
        | None -> failwith ("Couldn't find a dup in rucksack '" ^ rucksack ^ "'"))
  in
  let priority_sum =
    List.fold dupes ~init:0 ~f:(fun sum item -> sum + priority item)
  in
  printf "The total priority is %d.\n" priority_sum;
  ()
