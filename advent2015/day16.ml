open ContainersLabels
module StringMap = Map.Make (String)

let parse_and_add sues line =
  let pattern =
    Re.compile
    @@ Re.Pcre.re {|Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)|}
  in
  match Re.exec pattern line |> Re.Group.all with
  | [| _all; sue_num; attr1; val1; attr2; val2; attr3; val3 |] ->
      let the_sue =
        ( int_of_string sue_num,
          StringMap.of_list
            [
              (attr1, int_of_string val1);
              (attr2, int_of_string val2);
              (attr3, int_of_string val3);
            ] )
      in
      the_sue :: sues
  | _ -> sues

let satisfies detector_results what_i_remember =
  StringMap.for_all
    (fun k v ->
      match StringMap.get k detector_results with
      | Some v' -> v = v'
      | None -> true)
    what_i_remember

let run () =
  Utils.greet 16;

  let input = IO.(with_in (Utils.input_path_for_day 16) read_lines_l) in
  let sues = List.fold_left input ~init:[] ~f:parse_and_add in

  let detector_results =
    StringMap.of_list
      [
        ("children", 3);
        ("cats", 7);
        ("samoyeds", 2);
        ("pomeranians", 3);
        ("akitas", 0);
        ("vizslas", 0);
        ("goldfish", 5);
        ("trees", 3);
        ("cars", 2);
        ("perfumes", 1);
      ]
  in
  let the_sue_num, _attrs =
    List.find sues ~f:(fun (_num, attrs) -> satisfies detector_results attrs)
  in
  Printf.printf "The Sue that gave me the gift is %d.\n" the_sue_num;

  ()
