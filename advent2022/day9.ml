open Base
open Stdio

let day = 9

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of, equal]

    let delta_of_char = function
      | 'U' -> (0, 1)
      | 'D' -> (0, -1)
      | 'L' -> (-1, 0)
      | 'R' -> (1, 0)
      | _ -> failwith "Unrecognized direction"

    let move (x, y) (dx, dy) = (x + dx, y + dy)

    let delta_to_catch_up (x1, y1) (x2, y2) =
      if x1 = x2 && y1 = y2 then (0, 0)
      else if x1 = x2 && y1 < y2 - 1 then (0, 1)
      else if x1 = x2 && y1 > y2 + 1 then (0, -1)
      else if y1 = y2 && x1 < x2 - 1 then (1, 0)
      else if y1 = y2 && x1 > x2 + 1 then (-1, 0)
      else if (x1 < x2 - 1 && y1 < y2) || (x1 < x2 && y1 < y2 - 1) then (1, 1)
      else if (x1 > x2 + 1 && y1 < y2) || (x1 > x2 && y1 < y2 - 1) then (-1, 1)
      else if (x1 < x2 - 1 && y1 > y2) || (x1 < x2 && y1 > y2 + 1) then (1, -1)
      else if (x1 > x2 + 1 && y1 > y2) || (x1 > x2 && y1 > y2 + 1) then (-1, -1)
      else (0, 0)
  end

  include T
  include Comparator.Make (T)
end

module Rope = struct
  (* type t = Point.t list *)

  let init ?(len = 2) () = List.init len ~f:(fun _ -> (0, 0))

  let rec move t delta =
    match t with
    | [] -> []
    | hd :: follow :: tl ->
        let new_hd = Point.move hd delta in
        let follow_delta = Point.delta_to_catch_up follow new_hd in
        new_hd :: move (follow :: tl) follow_delta
    | [ hd ] -> [ Point.move hd delta ]

  let last t = List.last_exn t
end

let sequence_of_moves lines =
  let lines = Sequence.of_list lines in
  Sequence.concat_map lines ~f:(fun line ->
      let iterations =
        String.sub line ~pos:2 ~len:(String.length line - 2) |> Int.of_string
      in
      let direction = Point.delta_of_char line.[0] in
      Sequence.init iterations ~f:(fun _ -> direction))

let tests () =
  let input = [ "R 4"; "U 4"; "L 3"; "D 1"; "R 4"; "D 1"; "L 5"; "R 2" ] in
  let moves = sequence_of_moves input in
  let visited_points, _final_rope =
    Sequence.fold moves
      ~init:(Set.empty (module Point), Rope.init ())
      ~f:(fun (visited_points, rope) delta ->
        let new_rope = Rope.move rope delta in
        (Set.add visited_points (Rope.last new_rope), new_rope))
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
      ~f:(fun (visited_points, rope) delta ->
        let new_rope = Rope.move rope delta in
        (Set.add visited_points (Rope.last new_rope), new_rope))
  in
  printf "The tail of the rope visits %d positions at least once.\n"
    (Set.length visited_points);

  (* Part 2 *)
  let visited_points, _final_rope =
    Sequence.fold moves
      ~init:(Set.empty (module Point), Rope.init ~len:10 ())
      ~f:(fun (visited_points, rope) delta ->
        let new_rope = Rope.move rope delta in
        (Set.add visited_points (Rope.last new_rope), new_rope))
  in
  printf "The tail of the rope of length 10 visits %d positions at least once.\n"
    (Set.length visited_points)
