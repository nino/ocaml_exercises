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
  let matching_combinations =
    Sequence.count (all_sublists numbers) ~f:(fun sublist ->
        let sum = List.fold_left sublist ~f:( + ) ~init:0 in
        sum = 150)
  in
  Stdio.printf "There are %d combinations that sum to 150.\n"
    matching_combinations;

  ()
