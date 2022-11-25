open Containers

let incr_char char =
  match char with
  | 'a' .. 'y' -> Uchar.succ (Uchar.of_char char) |> Uchar.to_char
  | 'z' -> 'a'
  | _ -> failwith "Wrong char"

let rec incr_string string =
  if String.(string = "z") then "aa"
  else
    let len = String.length string in
    let last = String.get string (len - 1) in
    let new_last = incr_char last in
    let start = String.take (len - 1) string in
    if Char.(new_last = 'a') then incr_string start ^ String.of_char new_last
    else start ^ String.of_char new_last

let is_length_8 attempt = String.length attempt = 8

let contains_straight attempt =
  let full_straight = "abcdefghijklmnopqrstuvwxyz" in
  let len = String.length attempt in
  if len < 3 then false
  else
    Seq.range 0 (len - 3)
    |> Seq.exists (fun idx ->
           let sub = String.sub attempt idx 3 in
           String.find ~sub full_straight <> -1)

let contains_no_confusing_letters attempt =
  not
    String.(
      contains attempt 'o' || contains attempt 'i' || contains attempt 'l')

let contains_two_pairs attempt =
  let rec number_of_pairs acc str =
    let len = String.length str in
    if len < 2 then acc
    else if Char.(str.[0] = str.[1]) then
      number_of_pairs (acc + 1) (String.drop 2 str)
    else number_of_pairs acc (String.drop 1 str)
  in
  number_of_pairs 0 attempt = 2

let is_valid_password attempt =
  is_length_8 attempt && contains_straight attempt
  && contains_no_confusing_letters attempt
  && contains_two_pairs attempt

let find_next_password current_password =
  let rec loop attempt =
    if is_valid_password attempt then attempt
    else
      let next_try = incr_string attempt in
      if String.length next_try > 8 then "No password found" else loop next_try
  in
  loop current_password

let run () =
  Utils.greet 11;
  let input = "hxbxwxba" in
  let next_password = find_next_password input in
  Printf.printf "Santa's next password should be %s.\n" next_password
