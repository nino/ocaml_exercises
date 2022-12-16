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

let satisfies_with_ranges detector_results what_i_remember =
  (match StringMap.get "children" what_i_remember with
  | Some x -> x = StringMap.find "children" detector_results
  | None -> true)
  && (match StringMap.get "cats" what_i_remember with
     | Some x -> x > StringMap.find "cats" detector_results
     | None -> true)
  && (match StringMap.get "samoyeds" what_i_remember with
     | Some x -> x = StringMap.find "samoyeds" detector_results
     | None -> true)
  && (match StringMap.get "pomeranians" what_i_remember with
     | Some x -> x < StringMap.find "pomeranians" detector_results
     | None -> true)
  && (match StringMap.get "akitas" what_i_remember with
     | Some x -> x = StringMap.find "akitas" detector_results
     | None -> true)
  && (match StringMap.get "vizslas" what_i_remember with
     | Some x -> x = StringMap.find "vizslas" detector_results
     | None -> true)
  && (match StringMap.get "goldfish" what_i_remember with
     | Some x -> x < StringMap.find "goldfish" detector_results
     | None -> true)
  && (match StringMap.get "trees" what_i_remember with
     | Some x -> x > StringMap.find "trees" detector_results
     | None -> true)
  && (match StringMap.get "cars" what_i_remember with
     | Some x -> x = StringMap.find "cars" detector_results
     | None -> true)
  &&
  match StringMap.get "perfumes" what_i_remember with
  | Some x -> x = StringMap.find "perfumes" detector_results
  | None -> true

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

  let the_sue_num, _attrs =
    List.find sues ~f:(fun (_num, attrs) ->
        satisfies_with_ranges detector_results attrs)
  in
  Printf.printf "With the ranges thing, the Sue that gave me the gift is %d.\n"
    the_sue_num;

  ()
