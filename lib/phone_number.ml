(* None of this deals with unicode. *)
let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_punctuation c =
  (not (is_digit c))
  && (not (is_alpha c))
  && c <> '(' && c <> ')' && c <> '+' && c <> ' ' && c <> '.' && c <> '-'

exception HasLetters
exception HasPunctuation

let check_invalid_characters string =
  try
    (* I guess you could solve this more neatly with a recursive helper
       function, instead of exceptions. *)
    String.iter
      (fun c ->
        if is_alpha c then raise HasLetters
        else if is_punctuation c then raise HasPunctuation
        else ())
      string;
    Ok ()
  with
  | HasLetters -> Error "letters not permitted"
  | HasPunctuation -> Error "punctuations not permitted"

let cleanup input =
  let open String in
  let filtered = to_seq input |> Seq.filter is_digit |> of_seq in
  let len = length filtered in
  if len > 11 then Error "more than 11 digits"
  else if len < 10 then Error "incorrect number of digits"
  else if len = 11 && get filtered 0 <> '1' then
    Error "11 digits must start with 1"
  else
    let cleaned_up = if len = 11 then sub filtered 1 10 else filtered in
    let area_code_first_char = get cleaned_up 0 in
    let exchange_code_first_char = get cleaned_up 3 in
    if area_code_first_char = '0' then Error "area code cannot start with zero"
    else if area_code_first_char = '1' then
      Error "area code cannot start with one"
    else if exchange_code_first_char = '0' then
      Error "exchange code cannot start with zero"
    else if exchange_code_first_char = '1' then
      Error "exchange code cannot start with one"
    else Ok cleaned_up

let number input =
  match check_invalid_characters input with
  | Error err -> Error err
  | Ok () -> cleanup input
