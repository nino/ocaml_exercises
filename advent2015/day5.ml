open Base

let at_least_3_vowels string =
  let num_vowels =
    String.count string ~f:(function
      | 'a' | 'e' | 'i' | 'o' | 'u' -> true
      | _ -> false)
  in
  num_vowels >= 3

let at_least_one_double string =
  let len = String.length string in
  let idx = ref 0 in
  while !idx < len - 1 && Char.(string.[!idx] <> string.[!idx + 1]) do
    Int.incr idx
  done;
  !idx < len - 1

let no_forbidden_seqs string =
  List.for_all [ "ab"; "cd"; "pq"; "xy" ] ~f:(fun substring ->
      not (String.is_substring ~substring string))

let is_nice1 string =
  at_least_3_vowels string && at_least_one_double string
  && no_forbidden_seqs string

(* let sexp_of_set (s : (string, String.comparator_witness) Set.t) = *)
(*   List.sexp_of_t String.sexp_of_t (Set.to_list s) *)

let has_nonoverlapping_double_pair string =
  let len = String.length string in
  let rec loop prev_pairs current_pairs index =
    if index = len - 1 then false
    else
      let next_pair =
        String.of_char_list [ string.[index]; string.[index + 1] ]
      in
      if Set.mem prev_pairs next_pair then true
      else loop current_pairs (Set.add current_pairs next_pair) (index + 1)
  in
  let empty_set = Set.empty (module String) in
  loop empty_set empty_set 0

let has_xyx_pattern string =
  let len = String.length string in
  let idx = ref 0 in
  while !idx < len - 2 && Char.(string.[!idx] <> string.[!idx + 2]) do
    Int.incr idx
  done;
  !idx < len - 2

let is_nice2 string =
  has_nonoverlapping_double_pair string && has_xyx_pattern string

(* let test_nice_2 s should_be = *)
(*   Stdio.printf "“%s” should be %s, and is: %s\n" s should_be *)
(*     (if is_nice2 s then "nice" else "not nice") *)

let run () =
  Utils.greet 5;

  let input_path = Utils.input_path_for_day 5 in
  let inputs = Stdio.In_channel.read_lines input_path in

  let number_of_nice = List.count inputs ~f:is_nice1 in
  Stdio.printf "There are %d nice strings.\n" number_of_nice;

  let number_of_nice_new = List.count inputs ~f:is_nice2 in
  Stdio.printf "Under the new system, there are %d nice strings.\n"
    number_of_nice_new
