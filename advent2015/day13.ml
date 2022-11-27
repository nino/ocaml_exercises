open Containers
module StringSet = Set.Make (String)

type pair = string * string

(* TODO Need to find a way to make this work for all kinds of sets. *)
let rec permutations items =
  if StringSet.cardinal items = 1 then Seq.return (StringSet.elements items)
  else
    let cities_seq = StringSet.to_seq items in
    cities_seq
    |> Seq.flat_map (fun city ->
           let other_cities = StringSet.remove city items in
           let permutations_of_others = permutations other_cities in
           permutations_of_others
           |> Seq.map (fun permutation -> city :: permutation))

let parse happinesses people_tbl line =
  let pattern =
    Re.compile
    @@ Re.Pcre.re
         {|(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)\.|}
  in
  match Re.exec pattern line |> Re.Group.all with
  | [| _everything; name1; direction; amount; name2 |] ->
      let diff =
        if String.(direction = "gain") then int_of_string amount
        else -int_of_string amount
      in
      Hashtbl.replace happinesses (name1, name2) diff;
      Hashtbl.replace people_tbl name1 true;
      Hashtbl.replace people_tbl name2 true
  | _ -> ()

let rem a b = ((a mod b) + b) mod b

let happiness_for_arrangement happinesses arrangement =
  let arrangement' = Array.of_list arrangement in
  let happiness = ref 0 in
  for idx = 0 to Array.length arrangement' - 1 do
    let person = arrangement'.(idx) in
    let n1 = arrangement'.(rem (idx + 1) (Array.length arrangement')) in
    let n2 = arrangement'.(rem (idx - 1) (Array.length arrangement')) in
    happiness := !happiness + Hashtbl.get_or ~default:0 happinesses (person, n1);
    happiness := !happiness + Hashtbl.get_or ~default:0 happinesses (person, n2)
  done;
  !happiness

let run () =
  Utils.greet 13;

  let happinesses = Hashtbl.create 0 in
  let people_tbl = Hashtbl.create 0 in
  Printf.printf "so far.\n%!";
  let input_path = Utils.input_path_for_day 13 in
  Printf.printf "input_path = %s\n%!" input_path;
  IO.(
    with_in input_path (fun f ->
        Seq.iter (parse happinesses people_tbl) (read_lines_seq f)));
  let people =
    StringSet.add_seq (Hashtbl.to_seq_keys people_tbl) StringSet.empty
  in
  let greatest_happiness =
    Seq.fold
      (fun happiness arrangement ->
        let new_happiness = happiness_for_arrangement happinesses arrangement in
        Int.max happiness new_happiness)
      (-Int.max_int) (permutations people)
  in
  Printf.printf "Max happiness is %d.\n%!" greatest_happiness;

  let people = StringSet.add "Nino" people in
  let greatest_happiness =
    Seq.fold
      (fun happiness arrangement ->
        let new_happiness = happiness_for_arrangement happinesses arrangement in
        Int.max happiness new_happiness)
      (-Int.max_int) (permutations people)
  in
  Printf.printf "Max happiness when I'm there is %d.\n%!" greatest_happiness;

  ()
