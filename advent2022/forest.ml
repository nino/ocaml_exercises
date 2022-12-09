open Base

type t = string array

let of_string_list string_list =
  match string_list with
  | [] -> Ok [||]
  | first_row :: items ->
      let row_len = String.length first_row in
      if List.for_all items ~f:(fun row -> String.length row = row_len) then
        Ok (Array.of_list string_list)
      else Error (Error.of_string "Rows have unequal lengths")

let height t = Array.length t
let width t = try String.length t.(0) with _ -> 0
let get t row col = t.(row).[col]
let get_row t row = t.(row)

let get_col t col =
  Array.map t ~f:(fun row -> row.[col]) |> Array.to_list |> String.of_char_list

let is_visible t (row, col) =
  let the_tree = get t row col in
  let height = height t in
  let width = width t in
  let full_row = get_row t row in
  let full_col = get_col t col in
  let left = String.sub full_row ~pos:0 ~len:col in
  let right = String.sub full_row ~pos:(col + 1) ~len:(width - col - 1) in
  let top = String.sub full_col ~pos:0 ~len:row in
  let bottom = String.sub full_col ~pos:(row + 1) ~len:(height - row - 1) in
  String.for_all left ~f:(fun tree -> Char.(tree < the_tree))
  || String.for_all right ~f:(fun tree -> Char.(tree < the_tree))
  || String.for_all top ~f:(fun tree -> Char.(tree < the_tree))
  || String.for_all bottom ~f:(fun tree -> Char.(tree < the_tree))

let count_visible t =
  let all_tree_coordinates =
    Sequence.range 0 (height t)
    |> Sequence.concat_map ~f:(fun row ->
           Sequence.init (width t) ~f:(fun idx -> (row, idx)))
  in
  Sequence.count all_tree_coordinates ~f:(is_visible t)
