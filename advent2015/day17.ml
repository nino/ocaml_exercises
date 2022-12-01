open Base

let rec all_sublists list =
  match list with
  | [] -> Sequence.return []
  | hd :: tl ->
      let subsublists = all_sublists tl in
      Sequence.append subsublists
        (Sequence.map subsublists ~f:(fun sublist -> hd :: sublist))

let run () =
  Utils.greet 17;
  let input = Stdio.In_channel.read_lines (Utils.input_path_for_day 17) in
  let numbers = List.map input ~f:Int.of_string in
  let shortest_combination_length = ref Int.max_value in
  let matching_combinations =
    Sequence.count (all_sublists numbers) ~f:(fun sublist ->
        let sum = List.fold_left sublist ~f:( + ) ~init:0 in
        if sum = 150 then (
          shortest_combination_length :=
            Int.min !shortest_combination_length (List.length sublist);
          true)
        else false)
  in
  Stdio.printf "There are %d combinations that sum to 150.\n"
    matching_combinations;

  let matching_short_combinations =
    Sequence.count (all_sublists numbers) ~f:(fun sublist ->
        let sum = List.fold_left sublist ~f:( + ) ~init:0 in
        sum = 150 && List.length sublist = !shortest_combination_length)
  in
  Stdio.printf
    "There are %d combinations that are the shortest, and that's %d items long.\n"
    matching_short_combinations
    !shortest_combination_length;

  ()
