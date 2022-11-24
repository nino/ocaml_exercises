(* To make it weird, I'm doing this one without Base, and using Stdlib and a
   tiny bit of Containers instead. Maybe I should be using `open Containers`? *)

type city = string

module CityPair = struct
  type t = city * city

  let compare (a, b) (c, d) =
    let first = compare a c in
    if first <> 0 then first else compare b d
end

module DistanceMap = Map.Make (CityPair)
module CitySet = Set.Make (String)

let rec permutations cities =
  if CitySet.cardinal cities = 1 then Seq.return (CitySet.elements cities)
  else
    let cities_seq = CitySet.to_seq cities in
    cities_seq
    |> Seq.flat_map (fun city ->
           let other_cities = CitySet.remove city cities in
           let permutations_of_others = permutations other_cities in
           permutations_of_others
           |> Seq.map (fun permutation -> city :: permutation))

let distance_between ~distances city1 city2 =
  try DistanceMap.find (city1, city2) distances
  with _ -> DistanceMap.find (city2, city1) distances

let travel_distance distances cities =
  let rec loop already_traveled cities_left =
    match cities_left with
    | a :: b :: rest ->
        loop (already_traveled + distance_between ~distances a b) (b :: rest)
    | _ -> already_traveled
  in
  loop 0 cities

let smallest_distance distances permutations =
  Seq.fold_left
    (fun min permutation -> Int.min min (travel_distance distances permutation))
    Int.max_int permutations

let longest_distance distances permutations =
  Seq.fold_left
    (fun max permutation -> Int.max max (travel_distance distances permutation))
    0 permutations

let test_permutations () =
  let cities = CitySet.of_list [ "Hamburg"; "London"; "Bremen" ] in
  Seq.iter
    (fun list -> print_endline (String.concat ", " list))
    (permutations cities)

let get_cities_and_distances lines =
  let pattern = Re.compile @@ Re.Pcre.re {|(\w+) to (\w+) = (\d+)|} in
  lines
  |> List.fold_left
       (fun (cities, distances) line ->
         match Re.Pcre.exec ~rex:pattern line |> Re.Group.all with
         | [| _; a; b; dist |] ->
             let dist = int_of_string dist in
             ( CitySet.(cities |> add a |> add b),
               DistanceMap.add (a, b) dist distances )
         | _ -> (cities, distances))
       (CitySet.empty, DistanceMap.empty)

let test_test () =
  let inputs =
    [
      "London to Dublin = 464";
      "London to Belfast = 518";
      "Dublin to Belfast = 141";
    ]
  in
  let cities, distances = get_cities_and_distances inputs in
  print_int (smallest_distance distances (permutations cities));
  print_endline " should be 605."

let run () =
  Utils.greet 9;
  test_permutations ();
  test_test ();

  let input_path = Utils.input_path_for_day 9 in
  let lines = CCIO.(with_in input_path read_lines_l) in
  let cities, distances = get_cities_and_distances lines in
  let smallest_distance = smallest_distance distances (permutations cities) in
  Printf.printf "Smallest possible distance is %d\n" smallest_distance;
  let longest_distance = longest_distance distances (permutations cities) in
  Printf.printf "Longest possible distance is %d\n" longest_distance
