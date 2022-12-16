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

let left_of t row col =
  let full_row = get_row t row in
  String.sub full_row ~pos:0 ~len:col

let right_of t row col =
  let full_row = get_row t row in
  String.sub full_row ~pos:(col + 1) ~len:(width t - col - 1)

let top_of t row col =
  let full_col = get_col t col in
  String.sub full_col ~pos:0 ~len:row

let bottom_of t row col =
  let full_col = get_col t col in
  String.sub full_col ~pos:(row + 1) ~len:(height t - row - 1)

let is_visible t (row, col) =
  let the_tree = get t row col in
  let left = left_of t row col in
  let right = right_of t row col in
  let top = top_of t row col in
  let bottom = bottom_of t row col in
  String.for_all left ~f:(fun tree -> Char.(tree < the_tree))
  || String.for_all right ~f:(fun tree -> Char.(tree < the_tree))
  || String.for_all top ~f:(fun tree -> Char.(tree < the_tree))
  || String.for_all bottom ~f:(fun tree -> Char.(tree < the_tree))

let all_tree_coordinates t =
  Sequence.range 0 (height t)
  |> Sequence.concat_map ~f:(fun row ->
         Sequence.init (width t) ~f:(fun idx -> (row, idx)))

let count_visible t = Sequence.count (all_tree_coordinates t) ~f:(is_visible t)

let trees_visible_left t row col =
  let the_tree = get t row col in
  let left = left_of t row col in
  match
    String.findi (String.rev left) ~f:(fun _idx c -> Char.(c >= the_tree))
  with
  | Some (idx, _char) -> idx + 1
  | None -> col

let trees_visible_right t row col =
  let the_tree = get t row col in
  let right = right_of t row col in
  let width = width t in
  match String.findi right ~f:(fun _idx c -> Char.(c >= the_tree)) with
  | Some (idx, _char) -> idx + 1
  | None -> width - (col + 1)

let trees_visible_top t row col =
  let the_tree = get t row col in
  let top = top_of t row col in
  match
    String.findi (String.rev top) ~f:(fun _idx c -> Char.(c >= the_tree))
  with
  | Some (idx, _char) -> idx + 1
  | None -> row

let trees_visible_bottom t row col =
  let the_tree = get t row col in
  let bottom = bottom_of t row col in
  let height = height t in
  match String.findi bottom ~f:(fun _idx c -> Char.(c >= the_tree)) with
  | Some (idx, _char) -> idx + 1
  | None -> height - (row + 1)

let scenic_score t row col =
  trees_visible_left t row col
  * trees_visible_right t row col
  * trees_visible_top t row col
  * trees_visible_bottom t row col

let highest_scenic_score t =
  Sequence.map (all_tree_coordinates t) ~f:(fun (row, col) ->
      scenic_score t row col)
  |> Sequence.fold ~init:0 ~f:Int.max
