open Base
open Stdio

let day = 9

module Direction = struct
  type t = Up | Down | Left | Right

  let of_char = function
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith "Invalid direction."
end

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of, equal]

    let move (x, y) direction =
      match direction with
      | Direction.Up -> (x, y + 1)
      | Direction.Down -> (x, y - 1)
      | Direction.Left -> (x - 1, y)
      | Direction.Right -> (x + 1, y)

    let catch_up (x1, y1) (x2, y2) =
      if x1 = x2 && y1 = y2 then (x1, y1)
      else if x1 = x2 && y1 < y2 - 1 then (x1, y1 + 1)
      else if x1 = x2 && y1 > y2 + 1 then (x1, y1 - 1)
      else if y1 = y2 && x1 < x2 - 1 then (x1 + 1, y1)
      else if y1 = y2 && x1 > x2 + 1 then (x1 - 1, y1)
      else if (x1 < x2 - 1 && y1 < y2) || (x1 < x2 && y1 < y2 - 1) then
        (x1 + 1, y1 + 1)
      else if (x1 > x2 + 1 && y1 < y2) || (x1 > x2 && y1 < y2 - 1) then
        (x1 - 1, y1 + 1)
      else if (x1 < x2 - 1 && y1 > y2) || (x1 < x2 && y1 > y2 + 1) then
        (x1 + 1, y1 - 1)
      else if (x1 > x2 + 1 && y1 > y2) || (x1 > x2 && y1 > y2 + 1) then
        (x1 - 1, y1 - 1)
      else (x1, y1)
  end

  include T
  include Comparator.Make (T)
end

module Rope = struct
  type t = { head : Point.t; tail : Point.t }

  let init () = { head = (0, 0); tail = (0, 0) }
end

let sequence_of_moves lines =
  let lines = Sequence.of_list lines in
  Sequence.concat_map lines ~f:(fun line ->
      let iterations =
        String.sub line ~pos:2 ~len:(String.length line - 2) |> Int.of_string
      in
      let direction = Direction.of_char line.[0] in
      Sequence.init iterations ~f:(fun _ -> direction))

let tests () =
  let input = [ "R 4"; "U 4"; "L 3"; "D 1"; "R 4"; "D 1"; "L 5"; "R 2" ] in
  let moves = sequence_of_moves input in
  let visited_points, _final_rope =
    Sequence.fold moves
      ~init:(Set.empty (module Point), Rope.init ())
      ~f:(fun (visited_points, { head; tail }) direction ->
        let new_head = Point.move head direction in
        let new_tail = Point.catch_up tail new_head in
        (Set.add visited_points new_tail, { head = new_head; tail = new_tail }))
  in
  printf "Test: The tail of the rope visits %d positions at least once.\n"
    (Set.length visited_points)

let run () =
  tests ();
  let input = In_channel.read_lines (Utils.input_path_for_day 9) in
  let moves = sequence_of_moves input in
  let visited_points, _final_rope =
    Sequence.fold moves
      ~init:(Set.empty (module Point), Rope.init ())
      ~f:(fun (visited_points, { head; tail }) direction ->
        let new_head = Point.move head direction in
        let new_tail = Point.catch_up tail new_head in
        (Set.add visited_points new_tail, { head = new_head; tail = new_tail }))
  in
  printf "The tail of the rope visits %d positions at least once.\n"
    (Set.length visited_points)
