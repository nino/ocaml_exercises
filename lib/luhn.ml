open Base

let char_to_digit c = Char.to_int c - Char.to_int '0'

let contains_non_number_characters string =
  match String.find string ~f:(fun char -> not (Char.is_digit char)) with
  | Some _ -> true
  | None -> false

let is_checksum_valid string =
  let checksum =
    string |> String.to_list |> List.rev
    |> List.mapi ~f:(fun index c ->
           let num = char_to_digit c in
           if index % 2 = 1 then
             let new_num = num * 2 in
             if new_num > 9 then new_num - 9 else new_num
           else num)
    |> List.fold ~init:0 ~f:(fun total next_el -> total + next_el)
  in
  checksum % 10 = 0

let valid string =
  let stripped =
    String.filter string ~f:(fun c -> not (Char.is_whitespace c))
  in
  if contains_non_number_characters stripped then false
  else if String.length stripped <= 1 then false
  else is_checksum_valid stripped
